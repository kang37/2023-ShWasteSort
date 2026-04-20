# ============================================================================
# Combined R Analysis Script
# This script consolidates data loading, processing, descriptive statistics,
# SEM variable distribution plotting, and Structural Equation Modeling (SEM)
# from main_2.R, plot_var_distribution.R, and sem_single.R.
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Load Required Packages ----
# ----------------------------------------------------------------------------
# Ensure pacman is installed to manage other packages
pacman::p_load(
  dplyr, stringr, tidyr, patchwork, ggplot2, corrplot, readxl, purrr, showtext,
  lavaan, psych, semTools
)
showtext::showtext_auto()

# ----------------------------------------------------------------------------
# 2. Data Loading and Initial Processing (from main_2.R) ----
# ----------------------------------------------------------------------------
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
    print(paste("Reading data for year:", year))
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
    gender = na_if(gender, -2),
    gender = as.factor(gender),
    # 新增分组形式年龄变量。
    age_grp = case_when(
      age <= 25 ~ "<=25", age <= 40 ~ "25~40", age <= 60 ~ "40_60",
      age <= 100 ~ ">60", age >= 100 ~ NA_character_
    ),
    age_grp = factor(age_grp, levels = c("<=25", "25~40", "40_60", ">60")),
    # 新增教育水平分组。
    education_grp = case_when(
      education <= 3 ~ "under_senior", education == 4 ~ "undergrad",
      education == 5 ~ "beyond_master", education == 6 ~ NA_character_
    ),
    education_grp = factor(education_grp, levels = c(
      "under_senior", "undergrad", "beyond_master"
    ))
  ) %>%
  # 由于态度最后一个选项是“说不清楚”，因此反转后为“0”的项目要变成NA。
  mutate(ws_attitude = na_if(ws_attitude, 0))

# 用于作图的数据框：加入选项内容。
ws_full_text <- ws_full %>%
  mutate(
    gender = case_match(as.character(gender), "1" ~ "male", "2" ~ "female"), 
    education = case_match(
      as.character(education),
      "1" ~ "primary", "2" ~ "junior_high", "3" ~ "senior_high",
      c("4", "5") ~ "college_or_higher", "6" ~ "edu_other"
    ),
    occupation = case_match(
      as.character(occupation),
      "1" ~ "government", "2" ~ "institution", "3" ~ "state_enterprise",
      "4" ~ "foreign_enterprise", "5" ~ "private_enterprise", "6" ~ "self_employed",
      "7" ~ "freelancer", "8" ~ "retired", "9" ~ "student", "10" ~ "occ_other"
    ),
    residence_area = case_match(
      as.character(residence_area),
      "1" ~ "Pudong", "2" ~ "Huangpu", "3" ~ "Xuhui", "4" ~ "Changning",
      "5" ~ "Jing'an", "6" ~ "Putuo", "7" ~ "Hongkou", "8" ~ "Yangpu",
      "9" ~ "Minhang", "10" ~ "Baoshan", "11" ~ "Jiading", "12" ~ "Jinshan",
      "13" ~ "Songjiang", "14" ~ "Qingpu", "15" ~ "Fengxian", "16" ~ "Chongming"
    ),
    income = case_match(
      as.character(income),
      "1" ~ "≤ 3", "2" ~ "3~5", "3" ~ "5~10", "4" ~ "10~15",
      "5" ~ "15~20", "6" ~ "20~30", "7" ~ "> 30"
    )
  )

# 3. Basic Description: Respondents' Demographics (Table Output) ----
# 各年份样本数和样本总数。
table(ws_full$year)
sum(table(ws_full$year))

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
  )

# 合并年度数据和平均数据并输出为CSV
demographics_table <- bind_rows(gene_des_proc, gene_des_avg %>% mutate(year = "Average")) %>%
  select(variable, category, year, percentage) %>%
  pivot_wider(names_from = year, values_from = percentage) %>%
  mutate(
    across(
      .cols = where(is.numeric), .fns = ~ scales::percent(.x, accuracy = 0.01)
    )
  ) %>%
  mutate(category = factor(category, levels = order_levels)) %>%
  arrange(variable, category)

write.csv(demographics_table, "data_proc/demographics_table.csv", row.names = FALSE)

# 4. SEM Variables Distribution Plot ----

# Define variables and their labels
var_info <- tribble(
  ~var,                  ~label,                          ~latent,
  "satis_way_of_sh",    "Satisfaction with sorting",     "ATT",
  "ws_attitude",        "Attitude toward sorting",       "ATT",
  "pr_atten",           "Public publicity effect",       "SN",
  "regulate_law",       "Legal regulation effect",       "SN",
  "regulate_commu_rule", "Community rule effect",         "SN",
  "if_neighbor_ws",     "Neighbor influence",            "SN",
  "if_family_ws",       "Family influence",              "SN",
  "if_know_method",     "If know method",                "PBC",
  "if_sign_sort",       "If clear signs",                "PBC",
  "category_trouble",   "Too many categories",           "PBC",
  "wil_of_engage",      "Willingness (BI)",              "DV",
  "seper_recyc",        "Separation behavior (BEH)",     "DV"
)

# Calculate proportion of scores for each variable per year
all_vars <- var_info$var

long_data_sem <- ws_full %>%
  select(year, all_of(all_vars)) %>%
  pivot_longer(cols = all_of(all_vars), names_to = "variable", values_to = "score") %>%
  filter(!is.na(score)) %>%
  mutate(score = factor(as.integer(score))) %>%
  group_by(year, variable, score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(year, variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  left_join(var_info, by = c("variable" = "var"))

# Set facet order by latent variable group
long_data_sem$label <- factor(long_data_sem$label, levels = var_info$label)

# SEM相关变量各年份各分数选择比例条形图。
png(
  "data_proc/sem_var_distribution_by_year.png", 
  width = 1500, height = 1000, res = 300
)
ggplot(long_data_sem, aes(x = factor(year), y = prop, fill = score)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~ label, nrow = 3) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_fill_brewer(palette = "RdYlGn", direction = 1, name = "Score") +
  labs(x = "Year", y = "Proportion") +
  theme_minimal(base_size = 8) +
  theme(
    strip.text = element_text(face = "bold", size = 6),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
dev.off()
# 折线图。
plotly::ggplotly(
  ggplot(long_data_sem) + 
    geom_line(aes(year, prop, col = score, group = score)) + 
    facet_wrap(.~ variable) + 
    scale_color_brewer(palette = "RdYlGn", direction = 1, name = "Score")
)
# 对应数据。
long_data_sem %>% 
  filter(score == 4 | score == 5) %>% 
  group_by(variable, year) %>% 
  summarise(prop = sum(prop), .groups = "drop") %>% 
  mutate(prop = round(prop, digits = 2)) %>% 
  pivot_wider(
    id_cols = variable, names_from = year, values_from = prop
  ) 

# Group Differences in Intention and Behavior ----
# Dependent variables for difference analysis
target_vars_diff <- c("wil_of_engage", "seper_recyc")
# Grouping variables
grouping_vars <- c("gender", "age_grp", "education_grp")

# Function to perform statistical comparison (Wilcoxon or Kruskal-Wallis)
perform_group_comparison <- function(data, group_var, dep_var) {
  result_list <- lapply(unique(data$year), function(y) {
    df_year <- data %>%
      filter(year == y, !is.na(.data[[group_var]]), !is.na(.data[[dep_var]]))
    
    if (nrow(df_year) == 0) {
      return(NULL)
    }
    
    # Ensure the grouping variable is a factor for test
    df_year[[group_var]] <- factor(df_year[[group_var]])
    
    n_levels <- length(levels(df_year[[group_var]]))
    
    if (n_levels < 2) { # Not enough levels to compare
      return(NULL)
    }
    
    test_res <- if (n_levels == 2) {
      # Use formula interface for wilcox.test
      wilcox.test(formula(paste(dep_var, "~", group_var)), data = df_year)
    } else {
      # Use formula interface for kruskal.test
      kruskal.test(formula(paste(dep_var, "~", group_var)), data = df_year)
    }
    
    p_val <- test_res$p.value
    p_sig <- case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01 ~ "**",
      p_val < 0.05 ~ "*",
      TRUE ~ ""
    )
    
    tibble(
      year = y,
      group_variable = group_var,
      dependent_variable = dep_var,
      p_value = p_val,
      p_significance = p_sig,
      test_type = ifelse(n_levels == 2, "Wilcoxon Rank Sum Test", "Kruskal-Wallis Test")
    )
  })
  bind_rows(result_list)
}

# Function to calculate mean and median for each group level
calculate_group_stats <- function(data, group_var, dep_var) {
  data %>%
    filter(!is.na(.data[[group_var]]), !is.na(.data[[dep_var]])) %>%
    group_by(year, .data[[group_var]]) %>%
    summarise(
      mean_val = mean(.data[[dep_var]], na.rm = TRUE),
      median_val = median(.data[[dep_var]], na.rm = TRUE),
      .groups = "drop_last" # Keep year grouping for now
    ) %>%
    ungroup() %>% # Remove all grouping
    rename(group_level = as.name(group_var)) %>%
    mutate(
      group_variable = group_var,
      dependent_variable = dep_var
    ) %>%
    select(year, group_variable, dependent_variable, group_level, mean_val, median_val)
}

# Initialize lists to store results
all_group_diff_results <- list()
all_comparison_results <- list()
all_stats_results <- list()
counter <- 1

for (gv in grouping_vars) {
  for (dv in target_vars_diff) {
    cat(paste0("Analyzing group differences for ", dv, " by ", gv, "...\n"))
    
    # Perform statistical comparison
    comp_res <- perform_group_comparison(ws_full, gv, dv)
    
    # Calculate group-level statistics
    stats_res <- calculate_group_stats(ws_full, gv, dv)
    
    # Store comparison and stats results separately for plotting
    if (!is.null(comp_res) && nrow(comp_res) > 0) {
      all_comparison_results[[counter]] <- comp_res
    }
    all_stats_results[[counter]] <- stats_res
    
    # Combine comparison results with statistics
    if (!is.null(comp_res) && nrow(comp_res) > 0) {
      combined_yearly_results <- left_join(
        stats_res,
        comp_res %>% select(year, dependent_variable, p_value, p_significance, test_type),
        by = c("year", "dependent_variable")
      )
    } else {
      combined_yearly_results <- stats_res %>%
        mutate(p_value = NA_real_, p_significance = "", test_type = NA_character_)
    }
    
    # Calculate overall mean and median for each group_level (across years)
    overall_stats <- stats_res %>%
      group_by(group_variable, dependent_variable, group_level) %>%
      summarise(
        overall_mean = mean(mean_val, na.rm = TRUE),
        overall_median = median(median_val, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Combine yearly results with overall stats
    final_combined_res <- combined_yearly_results %>%
      left_join(overall_stats, by = c("group_variable", "dependent_variable", "group_level"))
    
    all_group_diff_results[[counter]] <- final_combined_res
    counter <- counter + 1
  }
}

# Bind all results into single data frames
final_group_diff_table <- bind_rows(all_group_diff_results) %>%
  arrange(group_variable, dependent_variable, year, group_level)
comparison_df <- bind_rows(all_comparison_results)
stats_df <- bind_rows(all_stats_results)

# Save the final table to CSV
write.csv(final_group_diff_table, "data_proc/group_differences_table.csv", row.names = FALSE)
cat("Group differences table saved to data_proc/group_differences_table.csv\n")

# --- Plot: Mean scores by group levels across years with significance markers ---
# Prepare data for mean score plot
mean_plot_data <- stats_df %>%
  mutate(
    group_var_label = case_when(
      group_variable == "gender" ~ "Gender",
      group_variable == "age_grp" ~ "Age Group",
      group_variable == "education_grp" ~ "Education Level",
      TRUE ~ group_variable
    ),
    dep_var_label = case_when(
      dependent_variable == "wil_of_engage" ~ "Intention",
      dependent_variable == "seper_recyc" ~ "Behavior",
      TRUE ~ dependent_variable
    ),
    # Clean up group level labels
    group_level_label = case_when(
      group_level == "1" ~ "Male",
      group_level == "2" ~ "Female",
      group_level == "<=25" ~ "<=25",
      group_level == "25~40" ~ "25~40",
      group_level == "40_60" ~ "40~60",
      group_level == ">60" ~ ">60",
      group_level == "under_senior" ~ "High School or Below",
      group_level == "undergrad" ~ "Undergraduate",
      group_level == "beyond_master" ~ "Graduate or Above",
      TRUE ~ as.character(group_level)
    )
  )
write.csv(mean_plot_data, "data_proc/mean_intension_behavior_of_group.csv")

# Prepare significance data for annotation
sig_data <- comparison_df %>%
  mutate(
    group_var_label = case_when(
      group_variable == "gender" ~ "Gender",
      group_variable == "age_grp" ~ "Age Group",
      group_variable == "education_grp" ~ "Education Level",
      TRUE ~ group_variable
    ),
    dep_var_label = case_when(
      dependent_variable == "wil_of_engage" ~ "Intention",
      dependent_variable == "seper_recyc" ~ "Behavior",
      TRUE ~ dependent_variable
    ),
    significant = p_value < 0.05
  )
write.csv(sig_data, "data_proc/mean_intension_behavior_of_group_statistic.csv")

# 各变量下意图和行为的差异，及其统计结果。
p_mean_scores <- ggplot(mean_plot_data) +
  # Add lines and points for mean scores
  geom_line(
    aes(x = factor(year), y = mean_val, color = group_level_label, group = group_level_label),
    linewidth = 0.8
  ) +
  geom_point(
    aes(x = factor(year), y = mean_val, color = group_level_label)
  ) +
  # Add significance stars at the top
  geom_text(
    data = sig_data,
    aes(x = factor(year), y = 4.8, label = p_significance),
    size = 4, color = "#E64B35", fontface = "bold", inherit.aes = FALSE
  ) +
  facet_grid(group_var_label ~ dep_var_label) +
  # scale_color_brewer(palette = "Set2", name = "Group Level") +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    x = "Year", y = "Mean Score",
    title = "Mean Scores of Intention and Behavior by Group Levels"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    strip.background = element_rect(fill = "gray90", color = "gray70"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5)
  )
p_mean_scores

# SEM Analysis and Coefficient Plot ----
# Define observed variables for latent constructs
att_vars <- c("satis_way_of_sh", "ws_attitude")
sn_vars  <- c("pr_atten", "regulate_law", "regulate_commu_rule",
               "if_neighbor_ws", "if_family_ws")
pbc_vars <- c("if_know_method", "if_sign_sort", "category_trouble")

# Define ordered variables and check
ordered_vars_all <- c(
  "satis_way_of_commu", "satis_way_of_sh", # satis_way_of_commu not in var_info
  "ws_attitude", "threat", # threat not in var_info
  "pr_atten", "if_no_ws_guilty", # if_no_ws_guilty not in var_info
  "regulate_law", "regulate_commu_rule",
  "if_neighbor_ws", "if_family_ws",
  "if_know_method", "if_sign_sort",
  "category_trouble", "time_cost_troub", # time_cost_troub not in var_info
  "wil_of_engage"
)

model_vars <- c(att_vars, pbc_vars, sn_vars)
ordered_vars_used <- intersect(ordered_vars_all, model_vars) # Only include variables actually in the model and data

# ----------------------------------------------------------------------------
# 5. Reliability and Validity Analysis (CFA) ----
# ----------------------------------------------------------------------------
cat("Performing Reliability and Validity Analysis...\n")

# Function to calculate Cronbach's Alpha for each construct
get_alpha <- function(df, vars, label) {
  res <- psych::alpha(df[, vars], check.keys = TRUE)
  return(data.frame(Construct = label, Alpha = res$total$raw_alpha))
}

# Function to calculate CR and AVE from lavaan fit (Handles Multi-group)
get_reliability_stats <- function(fit) {
  # compRelSEM returns CR (Composite Reliability)
  # AVE returns Average Variance Extracted
  # For multi-group, they typically return a MATRIX where columns are groups and rows are constructs
  cr_mat <- semTools::compRelSEM(fit)
  ave_mat <- semTools::AVE(fit)
  
  # Convert matrix to long-format data frame
  # Get group names (years)
  years <- colnames(cr_mat)
  constructs <- rownames(cr_mat)
  
  res_list <- list()
  for (y in years) {
    res_list[[y]] <- data.frame(
      year = y,
      Construct = constructs,
      CR = as.numeric(cr_mat[, y]),
      AVE = as.numeric(ave_mat[, y])
    )
  }
  
  res_all <- bind_rows(res_list)
  return(res_all)
}

# Calculate Reliability (Cronbach's Alpha) per year
reliability_results <- lapply(unique(ws_full$year), function(y) {
  df_year <- ws_full %>% filter(year == y)
  
  alpha_att <- get_alpha(df_year, att_vars, "ATT")
  alpha_sn  <- get_alpha(df_year, sn_vars, "SN")
  alpha_pbc <- get_alpha(df_year, pbc_vars, "PBC")
  
  bind_rows(alpha_att, alpha_sn, alpha_pbc) %>% mutate(year = y)
}) %>% bind_rows()

# Print Reliability Results
print(reliability_results)

# Perform CFA for Validity Analysis
cfa_model <- paste(
  paste("ATT =~", paste(att_vars, collapse = " + ")),
  paste("SN  =~", paste(sn_vars, collapse = " + ")),
  paste("PBC =~", paste(pbc_vars, collapse = " + ")),
  sep = "\n"
)

cat("Fitting CFA model...\n")
fit_cfa <- cfa(model = cfa_model, data = ws_full, group = "year", estimator = "WLSMV", ordered = ordered_vars_used)

# Extract CR and AVE
validity_results <- get_reliability_stats(fit_cfa)
print("Construct Reliability (CR) and AVE (Average Variance Extracted):")
print(validity_results)

# Build model string
build_sem_model <- function(att_vars, pbc_vars, sn_vars) {
  att_formula <- paste("ATT =~", paste(att_vars, collapse = " + "))
  sn_formula  <- paste("SN  =~", paste(sn_vars, collapse = " + "))
  pbc_formula <- paste("PBC =~", paste(pbc_vars, collapse = " + "))

  structural_paths <- "
  # 结构路径
  wil_of_engage ~ ATT + SN + PBC
  seper_recyc ~ wil_of_engage"

  # 局部误差协方差（根据变量存在与否添加）
  error_cov <- character(0)

  # Check if "satis_way_of_commu" is present in att_vars for error covariance,
  # although in the provided var_info, it's not listed.
  # Assuming if such variables were to be included in att_vars in a modified scenario.
  # For current var_info, this condition will be false.
  if (all(c("satis_way_of_commu", "satis_way_of_sh") %in% att_vars)) {
    error_cov <- c(error_cov, "satis_way_of_commu ~~ satis_way_of_sh")
  }
  if (all(c("category_trouble", "time_cost_troub") %in% pbc_vars)) {
    error_cov <- c(error_cov, "category_trouble ~~ time_cost_troub")
  }
  if (all(c("if_neighbor_ws", "if_family_ws") %in% sn_vars)) {
    error_cov <- c(error_cov, "if_neighbor_ws ~~ if_family_ws")
  }

  error_cov_section <- if (length(error_cov) > 0) {
    paste("
  
  # 局部误差协方差
  ", paste(error_cov, collapse = "
  "))
  } else {
    ""
  }

  paste0(
    "  # 测量模型
  ",
    att_formula, "
  ",
    sn_formula, "
  ",
    pbc_formula, "
",
    structural_paths,
    error_cov_section
  )
}

model_string <- build_sem_model(att_vars, pbc_vars, sn_vars)

cat("SEM Model Formula:")
cat(model_string)

check_vars <- c(model_vars, "wil_of_engage", "seper_recyc")
missing_vars <- setdiff(check_vars, names(ws_full))

if (length(missing_vars) > 0) {
  cat("The following variables are missing in ws_full data:
")
  print(missing_vars)
  stop("Missing required variables, cannot proceed with SEM.")
} else {
  cat("All SEM variables check passed!

")
}

# Fit the SEM model
cat("Fitting SEM model...
")

warnings_list_sem <- character(0)

fit <- withCallingHandlers(
  sem(
    model = model_string,
    data = ws_full,
    group = "year",
    estimator = "WLSMV",
    ordered = ordered_vars_used,
    parameterization = "theta"
  ),
  warning = function(w) {
    warnings_list_sem <<- c(warnings_list_sem, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)

if (length(warnings_list_sem) > 0) {
  cat("
Warnings during SEM fitting:
")
  for (w in warnings_list_sem) cat("  -", w, "
")
}

if (lavInspect(fit, "converged")) {
  cat("
SEM model successfully converged!

")

  # Fit indices
  fit_indices <- fitMeasures(fit, c(
    "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"
  ))
  cat("SEM Fit Indices:
")
  print(round(fit_indices, 3))

  # Path coefficients
  std_estimates <- parameterEstimates(fit, standardized = TRUE)
  path_results <- std_estimates %>%
    filter(op == "~", !is.na(std.all)) %>%
    select(group, lhs, rhs, est, se, z, pvalue, std.all)

  # Map group numbers to year labels
  year_labels <- c("2019", "2020", "2021", "2022", "2023") # Assuming group 1 is 2019, etc.
  path_results$year <- year_labels[path_results$group]

  cat("
Standardized Path Coefficients:
")
  print(path_results %>% select(year, lhs, rhs, std.all, pvalue))
  # Output results to CSV
  write.csv(
    path_results %>% select(year, lhs, rhs, std.all, pvalue),
    "data_proc/sem_path_coefficients.csv", row.names = FALSE
  )
  cat("SEM path coefficients saved to data_proc/sem_path_coefficients.csv
")

  # Plot path coefficient changes
  plot_data_sem_paths <- path_results %>%
    filter(lhs %in% c("wil_of_engage", "seper_recyc")) %>%
    mutate(
      Path_full = case_when(
        lhs == "wil_of_engage" & rhs == "ATT" ~ "Attitude -> Intention",
        lhs == "wil_of_engage" & rhs == "SN"  ~ "Subjective Norm -> Intention",
        lhs == "wil_of_engage" & rhs == "PBC" ~ "Perceived Control -> Intention",
        lhs == "seper_recyc" & rhs == "wil_of_engage" ~ "Intention -> Behavior",
        TRUE ~ "Other"
      ),
      Significant = factor(
        ifelse(pvalue < 0.05, "Significant (p < 0.05)", "Not significant"),
        levels = c("Significant (p < 0.05)", "Not significant")
      ),
      year_num = as.numeric(year)
    ) %>%
    filter(Path_full != "Other") %>%
    mutate(Path_full = factor(Path_full, levels = c(
      "Attitude -> Intention",
      "Subjective Norm -> Intention",
      "Perceived Control -> Intention",
      "Intention -> Behavior"
    )))

  path_colors <- c(
    "Attitude -> Intention"           = "#4DBB15",
    "Subjective Norm -> Intention"    = "#4DBBD5",
    "Perceived Control -> Intention"  = "#00A087",
    "Intention -> Behavior"           = "#E64B35"
  )

  plot_data_sem_paths <- plot_data_sem_paths %>%
    mutate(
      point_fill = ifelse(
        pvalue < 0.05,
        path_colors[as.character(Path_full)],
        "white"
      )
    )

  p_sem_paths <- ggplot(plot_data_sem_paths, aes(x = year_num, y = std.all, color = Path_full)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
    geom_line(linewidth = 0.9) +
    geom_point(aes(fill = point_fill, shape = Significant), size = 3.5, stroke = 1) +
    facet_wrap(~ Path_full, ncol = 1) +
    scale_x_continuous(breaks = 2019:2023) +
    scale_color_manual(values = path_colors, guide = "none") +
    scale_shape_manual(values = c("Significant (p < 0.05)" = 21, "Not significant" = 21)) +
    scale_fill_identity() +
    labs(
      x = "Year",
      y = "Standardized Coefficient"
    ) +
    theme_classic(base_size = 9) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 9, margin = margin(b = 10)),
      axis.text.x = element_text(angle = 90),
      strip.background = element_rect(fill = "gray85", color = "gray50"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.5)
    ) +
    guides(
      shape = guide_legend(override.aes = list(
        size = 3.5, fill = c("black", "white"), color = "black"
      ))
    )

  ggsave("data_proc/single_model_path_plot.pdf", plot = p_sem_paths, width = 12, height = 4)
  cat("SEM path coefficient plot saved to data_proc/single_model_path_plot.pdf
")

} else {
  cat("
SEM model did not converge.
")
}

# ----------------------------------------------------------------------------
# 7. Spillover Effect Analysis: Correlation between Waste Sorting and Other Environmental Behaviors ----
# ----------------------------------------------------------------------------

# Function to detect correlation between two variables year by year.
# Uses Kendall's Tau correlation coefficient.
get_cor <- function(data, var_x, var_y) {
  result_list <- lapply(
    unique(data$year),
    function(y) {
      # Extract target subset data.
      tar_df <- data %>%
        filter(year == y, !is.na(.data[[var_x]]), !is.na(.data[[var_y]]))

      if (nrow(tar_df) > 1) { # Need at least 2 non-NA observations to calculate correlation
        res_stat <-
          cor.test(tar_df[[var_x]], tar_df[[var_y]], method = "kendall")
        res <- tibble(
          year = y,
          var_1 = var_x,
          var_2 = var_y,
          correlation_tau = res_stat$estimate,
          p_value = res_stat$p.value
        )
      } else {
        res <- tibble(
          year = y,
          var_1 = var_x,
          var_2 = var_y,
          correlation_tau = NA_real_,
          p_value = NA_real_
        )
      }
      return(res)
    }
  ) %>%
    bind_rows() %>%
    mutate(p_significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ))
}

# Define waste sorting related variables (intention and behavior)
waste_sorting_vars <- c("seper_recyc") # wil_of_engage: Willingness to engage, seper_recyc: Separation behavior

# Define other environmental behaviors
other_env_behaviors <- c("reuse_bag", "energy_concern", "save_energy", "share_often", "freq_online_secondhand", "volun_expr")

# Initialize an empty list to store all spillover correlation results
all_spillover_results <- list()
spillover_counter <- 1

for (ws_var in waste_sorting_vars) {
  for (env_var in other_env_behaviors) {
    cat(paste0("Calculating correlation between ", ws_var, " and ", env_var, "...\n"))
    current_cor_results <- get_cor(ws_full, ws_var, env_var)
    all_spillover_results[[spillover_counter]] <- current_cor_results
    spillover_counter <- spillover_counter + 1
  }
}

# Combine all spillover correlation results into a single data frame
final_spillover_table <- bind_rows(all_spillover_results) %>%
  arrange(var_1, var_2, year)

# Save the final table to CSV
write.csv(final_spillover_table, "data_proc/spillover_correlation_table.csv", row.names = FALSE)

