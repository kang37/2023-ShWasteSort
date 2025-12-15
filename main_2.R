# Statement ----
# 上海市垃圾分类处理。

# Preparation ----
pacman::p_load(
  dplyr, tidyr, patchwork, ggplot2, corrplot, readxl, purrr, showtext
)
showtext::showtext_auto()

# Data ----
# 读取列名对应表
colname_mapping <- read_excel("data_raw/colname_mapping.xlsx")

# 创建列名重命名函数
create_rename_vector <- function(mapping_df, year) {
  col_year <- paste0("col_", year)
  mapping_year <- mapping_df %>%
    filter(!is.na(.data[[col_year]])) %>%
    select(old_name = all_of(col_year), new_name = unified_name_en)
  setNames(mapping_year$old_name, mapping_year$new_name)
}

# 批量读取数据
years <- 2019:2023
ws_list <- map2(
  paste0("data_raw/SHWS", years, ".xlsx"), 
  years, 
  function(file, year) {
    read_excel(file) %>%
      rename(any_of(create_rename_vector(colname_mapping, year))) %>% 
      mutate(year = as.factor(year))
  }
)

# 所有需要重编码的变量（合并为一个向量）
vars_2019_recode <- c(
  # 信息质量 (11个)
  "info_qual_tv", "info_qual_news", "info_qual_web", "info_qual_ad", 
  "info_qual_gov", "info_qual_school", "info_qual_street", 
  "info_qual_volunteer", "info_qual_neighbor", "info_qual_family", 
  "info_qual_property",
  # 联系频率 (10个)
  "freq_online_secondhand", "freq_gov", "freq_ngo", "freq_media", 
  "freq_school", "freq_waste_co", "freq_resident", "freq_street", 
  "freq_property", "freq_supervisor",
  # 部门作用 (9个)
  "role_gov", "role_ngo", "role_media", "role_school", "role_waste_co",
  "role_resident", "role_street", "role_property", "role_supervisor",
  # 部门支持 (9个)
  "support_gov", "support_ngo", "support_media", "support_school",
  "support_waste_co", "support_resident", "support_street", 
  "support_property", "support_supervisor"
)

# 通用重编码函数：a→1, b→2, c→3, d→4, e→5, -3→NA
recode_2019_letters <- function(x) {
  case_when(
    str_detect(x, "^a") ~ 1,  # 以a开头的任何字符串 → 1
    str_detect(x, "^b") ~ 2,  # 以b开头的任何字符串 → 2
    str_detect(x, "^c") ~ 3,  # 以c开头的任何字符串 → 3
    str_detect(x, "^d") ~ 4,  # 以d开头的任何字符串 → 4
    str_detect(x, "^e") ~ 5,  # 以e开头的任何字符串 → 5
    x == "-3" | x == -3 ~ NA_real_,  # -3 → NA
    TRUE ~ as.numeric(x)      # 其他情况转为数字（如果已经是数字）
  )
}

# 一次性处理所有变量。
ws_list[[1]] <- ws_list[[1]] %>%
  mutate(across(any_of(vars_2019_recode), recode_2019_letters))

# 处理其他有问题的变量。
# 老人数量。
# Bug：还要删除异常值。
ws_list[[2]] <- ws_list[[2]] %>% 
  mutate(across(any_of("elder_num"), as.numeric))
ws_list[[4]] <- ws_list[[4]] %>% 
  mutate(across(any_of("elder_num"), as.numeric))
ws_list[[5]] <- ws_list[[5]] %>% 
  mutate(across(any_of("elder_num"), as.numeric))
# 年龄。
ws_list[[4]] <- ws_list[[4]] %>% 
  mutate(across(any_of("age"), as.numeric))

# 生成完整数据框。
# Bug：有些NA值需要去除，如性别中的“-2”。
ws_full <- bind_rows(ws_list) %>% 
  # 对部分李克特变量进行反向编码：原1-5变成5-1。
  mutate(
    across(
      .cols = all_of(
        colname_mapping %>%
          filter(var_scale5_rev == 1) %>%
          pull(unified_name_en) 
      ), 
      .fns = ~ {
        # 确保列是数值型，否则反转公式无效
        x <- as.numeric(.x) 
        # 反转公式：max_value + 1 - x
        # 1 -> 5+1-1 = 5
        # 5 -> 5+1-5 = 1
        # NA -> 5+1-NA = NA
        5 + 1 - x
      }
    )
  ) %>% 
  mutate(
    # 转化性别数据类型。
    gender = case_when(gender == "-2" ~ NA, TRUE ~ gender), 
    gender = as.factor(gender), 
    # 新增分组形式年龄变量。
    age_grp = case_when(
      age <= 25 ~ "<=25", age <= 40 ~ "25~40", age <= 60 ~ "40_60", 
      age <= 100 ~ ">60", age >= 100 ~ NA
    ), 
    age_grp = factor(age_grp, levels = c("<=25", "25~40", "40_60", ">60")), 
    # 新增教育水平分组。
    # Bug: education本身是否转化成factor？
    education_grp = case_when(
      education <= 3 ~ "under_senior", education == 4 ~ "undergrad", 
      education == 5 ~ "beyond_master", education == 6 ~ NA
    ), 
    education_grp = factor(education_grp, levels = c(
      "under_senior", "undergrad", "beyond_master"
    ))
  )

# 用于作图的数据框：加入选项内容。
ws_full_text <- ws_full %>% 
  mutate(
    gender = case_match(gender, 1 ~ "male", 2 ~ "female"), 
    education = case_match(
      education, 
      1 ~ "primary", 2 ~ "junior_high", 3 ~ "senior_high", 
      c(4, 5) ~ "college_or_higher", 6 ~ "edu_other"
    ), 
    occupation = case_match(
      occupation, 
      1 ~ "government", 2 ~ "institution", 3 ~ "state_enterprise", 
      4 ~ "foreign_enterprise", 5 ~ "private_enterprise", 6 ~ "self_employed", 
      7 ~ "freelancer", 8 ~ "retired", 9 ~ "student", 10 ~ "occ_other"
    ), 
    residence_area = case_match(
      residence_area, 
      1 ~ "Pudong", 2 ~ "Huangpu", 3 ~ "Xuhui", 4 ~ "Changning", 
      5 ~ "Jing'an", 6 ~ "Putuo", 7 ~ "Hongkou", 8 ~ "Yangpu", 
      9 ~ "Minhang", 10 ~ "Baoshan", 11 ~ "Jiading", 12 ~ "Jinshan", 
      13 ~ "Songjiang", 14 ~ "Qingpu", 15 ~ "Fengxian", 16 ~ "Chongming"
    ), 
    income = case_match(
      income, 
      1 ~ "≤ 3", 2 ~ "3~5", 3 ~ "5~10", 4 ~ "10~15", 
      5 ~ "15~20", 6 ~ "20~30", 7 ~ "> 30"
    )
  )

# General description ----
# 获取所有变量的原始顺序向量。
order_levels <- list(
  gender = c("male", "female"),
  education = c("primary", "junior_high", "senior_high", "college_or_higher", "edu_other"),
  occupation = c("government", "institution", "state_enterprise", "foreign_enterprise", "private_enterprise", "self_employed", "freelancer", "retired", "student", "occ_other"),
  income = c("≤ 3", "3~5", "5~10", "10~15", "15~20", "20~30", "> 30")
) %>% 
  # 将所有可能的分类合并成一个总的顺序向量
  unlist(., use.names = FALSE)

# --- 2. 数据处理：计算百分比和多年平均值 (列名改为小写) ---
gene_des_proc <- ws_full_text %>%  
  pivot_longer(
    cols = c(gender, education, occupation, income),
    names_to = "variable", # 列名小写
    values_to = "category" # 列名小写
  ) %>%
  # 计数并计算年度百分比
  count(year, variable, category, name = "count") %>% # n改为count
  group_by(year, variable) %>%
  mutate(
    percentage = count / sum(count), # 列名小写
    label = scales::percent(percentage, accuracy = 2) 
  ) %>%
  ungroup()

# --- 3. 计算多年平均值并合并到数据中 ---
gene_des_avg <- gene_des_proc %>%
  group_by(variable, category) %>%
  summarise(
    percentage = mean(percentage), # 计算多年平均百分比
    label = scales::percent(percentage, accuracy = 2),
    .groups = 'drop'
  ) %>%
  mutate(
    year = "Average", # 设置 year 为 'Average'，作为新的 x 轴刻度
    count = NA_integer_ # 填充 NA
  )

# 合并年度数据和平均数据
bind_rows(gene_des_proc, gene_des_avg) %>% 
  # 4. Y轴排序：将 category 转换为因子，并应用预先设定的顺序
  mutate(category = factor(category, levels = rev(order_levels))) %>% 
  # 作图。
  ggplot(aes(x = factor(year), y = category, fill = percentage)) + 
  geom_tile(color = "white") + # 添加白色边框，增强区分度
  geom_text(aes(label = label), color = "white", size = 3) +
  facet_wrap(.~ variable, scales = "free_y", ncol = 2) + 
  scale_fill_gradient(high = "red", low = "darkgreen") +
  labs(y = "Category", fill = "Percentage") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 8),
    strip.text = element_text(face = "bold")
  )

# 输出表格。
bind_rows(gene_des_proc, gene_des_avg) %>%
  select(variable, category, year, percentage) %>% 
  pivot_wider(names_from = year, values_from = percentage) %>%
  # 如果需要进行后续计算，请跳过此步骤
  mutate(
    across(
      .cols = where(is.numeric), .fns = ~ scales::percent(.x, accuracy = 0.01) 
    )
  ) %>% 
  mutate(category = factor(category, levels = order_levels)) %>% 
  arrange(category) %>% 
  write.csv("data_proc/gene_des_table.csv")

# Correlation ----
# 目标变量列表
target_vars <- c("wil_of_engage", "willing_share", "seper_recyc", "share_often")

# 目标变量概况。
ws_full %>% 
  select("year", target_vars) %>% 
  pivot_longer(
    cols = all_of(target_vars), names_to = "question", values_to = "reply"
  ) %>% 
  group_by(year, question, reply) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(question = factor(question, levels = target_vars)) %>% 
  ggplot() + 
  geom_col(aes(year, n, fill = as.character(reply)), position = "fill") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_bw() + 
  facet_wrap(. ~ question) + 
  labs(fill = "Likert scale", x = NULL, y = "Proportion") + 
  theme(
    strip.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# 函数：返回一个整洁的data.frame。
calculate_and_tidy_cor <- function(data_subset) {
  # 1. 提取目标变量并转换为矩阵
  cor_data <- data_subset %>% select(all_of(target_vars))
  
  if(nrow(cor_data) < 2) {
    return(tibble(Var1 = character(), Var2 = character(), r = numeric(), P = numeric()))
  }
  
  # 2. 计算相关矩阵
  cor_res <- rcorr(as.matrix(cor_data), type = "spearman")
  
  # 3. 将 R 值和 P 值转换为 data.frame，然后进行整洁化
  
  # R 值矩阵转为长格式
  r_tidy <- cor_res$r %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "Var1") %>%
    pivot_longer(
      cols = -Var1, 
      names_to = "Var2", 
      values_to = "R_value"
    )
  
  # P 值矩阵转为长格式
  p_tidy <- cor_res$P %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "Var1") %>%
    pivot_longer(
      cols = -Var1, 
      names_to = "Var2", 
      values_to = "P_value"
    )
  
  # 合并 R 值和 P 值
  tidy_cor_table <- left_join(r_tidy, p_tidy, by = c("Var1", "Var2")) %>%
    
    # 筛选：移除对角线 (自己和自己相关) 和重复的配对 (只保留 Var1 < Var2)
    filter(Var1 != Var2, Var1 < Var2) %>% 
    
    # 格式化 P 值 (使用科学记数法)
    mutate(
      P_value_formatted = format.pval(
        P_value, 
        digits = 3, 
        eps = 1e-15, 
        scientific = TRUE
      )
    )
  
  return(tidy_cor_table)
}

# 应用函数并生成最终数据框。
final_correlation_data <- ws_full %>%
  # 嵌套数据：按 'year' 分组
  group_by(year) %>%
  nest() %>%
  
  # 应用函数：对每个年份的数据子集应用函数，并生成新的 'tidy_cor' 列
  mutate(
    tidy_cor = map(data, calculate_and_tidy_cor)
  ) %>%
  
  # 展开：将嵌套的 'tidy_cor' data.frame 展开到主数据框中
  unnest(tidy_cor) %>%
  
  # 创建一个配对标识符，用于绘图分组
  mutate(
    Pair = paste(Var1, Var2, sep = " ~ ")
  ) %>%
  select(year, Pair, R_value, P_value, P_value_formatted)

# 结果输出。
# 表格输出
correlation_table <- final_correlation_data %>%
  select(
    Year = year,
    Variable_Pair = Pair,
    Correlation_R = R_value,
    P_value_Text = P_value_formatted
  ) %>%
  arrange(Year, Variable_Pair)
# 相关结果表格。
print(correlation_table)

# 转化成宽表格。
# 增加函数：根据 p 值生成星号
get_stars <- function(p_value) {
  # 按照您的要求设置显著性分级
  if (is.na(p_value)) {
    return("")
  } else if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("") # 不显著时不显示星号
  }
}
final_correlation_data %>%
  # 1. 计算星号标记
  mutate(
    # 应用自定义函数生成星号列
    Significance_Stars = map_chr(P_value, get_stars),
    # 2. 合并 R 值和星号：将 R 值四舍五入到两位小数并拼接星号
    R_with_Stars = paste0(round(R_value, 2), Significance_Stars)
  ) %>%
  # 3. 准备 pivot_wider
  select(year, Pair, R_with_Stars) %>%
  # 4. 转换为宽表格：将年份作为新的列名
  pivot_wider(
    names_from = year,
    values_from = R_with_Stars
  ) %>%
  # 5. 重命名列以提高可读性
  rename(Variable_Pair = Pair)

# 折线图输出。
ggplot(
  final_correlation_data, 
  aes(x = year, y = R_value, group = Pair)
) +
  geom_line() +
  geom_point(size = 2) +
  # 添加零线作为参考
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    x = "年份 (Year)", y = "斯皮尔曼相关系数 (Rho)"
  ) +
  facet_wrap(.~ Pair) + 
  theme_bw() + 
  theme(legend.position = "bottom")

# Group difference ----
# 不同性别、年龄、职业人群分类意愿和分类行为差异。
# 函数：统计检验不同分组之间的变量差异。
comp_var <- function(var_x, var_y) {
  lapply(
    2019:2023, 
    function(x) {
      # 统计分析：根据自变量水平数选择分析方法。
      if(length(levels(ws_full[var_x])) == 2) {
        res <- wilcox.test(
          get(var_y) ~ get(var_x), data = ws_full %>% filter(year == x)
        )
      } else {
        res <- kruskal.test(
          get(var_y) ~ get(var_x), data = ws_full %>% filter(year == x)
        )
      }
      # 输出所需结果。
      data.frame(year = x, statistic = res$statistic, p = res$p.value)
    }
  ) %>% 
    bind_rows() %>% 
    mutate(p_sig = case_when(
      p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p >= 0.05 ~ ""
    ))
}

# 不同分组之间意愿差异。
comp_var("gender", "wil_of_engage")
comp_var("age_grp", "wil_of_engage")
comp_var("education_grp", "wil_of_engage")
# 不同分组之间行为差异。
comp_var("gender", "seper_recyc")
comp_var("age_grp", "seper_recyc")
comp_var("education_grp", "seper_recyc")

# 函数：计算各组人员中位数等指标。
calc_mid <- function(var_x, var_y) {
  # 将字符串参数转换为符号，以便使用{{ }}引用。
  var_x_sym <- sym(var_x) 
  var_y_sym <- sym(var_y)
  # 计算各年份各组中位数和平均秩次。
  res <- ws_full %>%
    # 去掉NA值的行。
    filter(!is.na({{ var_x_sym }})) %>% 
    # 计算总秩次。
    group_by(year) %>% 
    # 使用 {{ var_y_sym }} 确保在 mutate 中正确引用变量
    mutate(global_rank = rank({{ var_y_sym }}) ) %>% 
    ungroup() %>% 
    # 计算各年份各组中位数和平均秩次。
    group_by(year, {{ var_x_sym }}) %>% 
    summarise(
      mid = median({{ var_y_sym }}, na.rm = TRUE), 
      mean_rank = mean(global_rank, na.rm = TRUE),
      .groups = "drop"
    )
  print(
    ggplot(res) + 
      geom_point(
        aes(year, mean_rank, col = {{ var_x_sym }}), alpha = 0.8, size = 3
      )
  )
  return(res)
}
# 意愿差异。
lapply(
  c("gender", "age_grp", "education_grp"), 
  function(x) calc_mid(x, "wil_of_engage")
) 
# 行为差异。
lapply(
  c("gender", "age_grp", "education_grp"), 
  function(x) calc_mid(x, "seper_recyc")
) 
