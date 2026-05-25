# Packages ----
pacman::p_load(
  dplyr, stringr, tidyr, patchwork, ggplot2, corrplot, readxl, purrr, showtext,
  psych, seminr
)
showtext::showtext_auto()

# Data Loading and Initial Processing ----
# 各年份问卷问题映射表。
colname_mapping <- read_excel("data_raw/colname_mapping.xlsx")

# 函数：读取问卷问题映射表，为指定年份生成原始列名到统一英文名的重命名映射向量，使跨年字段名统一。
create_rename_vector <- function(mapping_df, year) {
  col_year <- paste0("col_", year)
  mapping_year <- mapping_df %>%
    filter(!is.na(.data[[col_year]])) %>%
    select(old_name = all_of(col_year), new_name = unified_name_en)
  setNames(mapping_year$old_name, mapping_year$new_name)
}

# 后续分析所需的全部统一英文列名（bind_rows 前预先筛选，避免跨年类型冲突）
vars_needed <- c(
  # 元数据
  "year", 
  # 态度 (ATT)
  "ws_attitude", "ws_interest", "threat",
  "satis_way_of_sh", "satis_way_of_commu",
  # 指令性规范 (INJ_NORM)
  "pr_atten", "regulate_law", "regulate_commu_rule", "if_no_ws_guilty",
  # 描述性规范 (DESC_NORM)
  "if_neighbor_ws", "if_family_ws",
  # 感知行为控制 (PBC)
  "if_know_method", "if_sign_sort", "category_trouble", "time_cost_troub",
  # 行为意图与行为
  "wil_of_engage", "seper_recyc",
  # 溢出效应变量（仅部分年份存在，用 any_of 处理）
  "reuse_bag", "energy_concern", "save_energy",
  "share_often", "freq_online_secondhand", "volun_expr",
  # 人口统计
  "gender", "age", "education", "occupation",
  "income", "family_size", "elder_num",
  "hukou", "residence_area", "residence_commu"
)

# 读取各年份数据。
years <- 2019:2023
ws_full <- map2(
  paste0("data_raw/SHWS", years, ".xlsx"),
  years,
  function(file, year) {
    print(paste("Reading data for year:", year))
    read_excel(file) %>%
      rename(any_of(create_rename_vector(colname_mapping, year))) %>%
      mutate(year = as.factor(year)) %>%
      mutate(across(any_of(c("elder_num", "age")), as.numeric)) %>%
      select(any_of(vars_needed))
  }
) %>% 
  # 合并数据。
  bind_rows() %>%
  mutate(
    across(
      .cols = any_of(
        colname_mapping %>%
          filter(var_scale5_rev == 1) %>%
          pull(unified_name_en)
      ),
      .fns = ~ {
        x <- as.numeric(.x)
        5 + 1 - x
      }
    )
  ) %>%
  mutate(
    gender = na_if(gender, -2),
    gender = as.factor(gender),
    age_grp = case_when(
      age <= 25 ~ "<=25", age <= 40 ~ "25~40", age <= 60 ~ "40_60",
      age <= 100 ~ ">60", age >= 100 ~ NA_character_
    ),
    age_grp = factor(age_grp, levels = c("<=25", "25~40", "40_60", ">60")),
    education_grp = case_when(
      education <= 3 ~ "under_senior", education == 4 ~ "undergrad",
      education == 5 ~ "beyond_master", education == 6 ~ NA_character_
    ),
    education_grp = factor(education_grp, levels = c(
      "under_senior", "undergrad", "beyond_master"
    ))
  ) %>%
  mutate(ws_attitude = na_if(ws_attitude, 0))

# 重编码。
# Bug：是否可以精简？
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

# Respondents' Demographics ----
table(ws_full$year)

order_levels <- list(
  gender = c("male", "female"),
  education = c("primary", "junior_high", "senior_high", "college_or_higher", "edu_other"),
  occupation = c("government", "institution", "state_enterprise", "foreign_enterprise", "private_enterprise", "self_employed", "freelancer", "retired", "student", "occ_other"),
  income = c("≤ 3", "3~5", "5~10", "10~15", "15~20", "20~30", "> 30")
) %>%
  unlist(., use.names = FALSE)

gene_des_proc <- ws_full_text %>%
  pivot_longer(
    cols = c(gender, education, occupation, income),
    names_to = "variable",
    values_to = "category"
  ) %>%
  count(year, variable, category, name = "count") %>%
  group_by(year, variable) %>%
  mutate(
    percentage = count / sum(count),
    label = scales::percent(percentage, accuracy = 2)
  ) %>%
  ungroup()

gene_des_avg <- gene_des_proc %>%
  group_by(variable, category) %>%
  summarise(
    percentage = mean(percentage),
    label = scales::percent(percentage, accuracy = 2),
    .groups = 'drop'
  )

demographics_table <- bind_rows(
  gene_des_proc, gene_des_avg %>% mutate(year = "Average")
) %>%
  select(variable, category, year, percentage) %>%
  pivot_wider(names_from = year, values_from = percentage) %>%
  mutate(
    across(
      .cols = where(is.numeric), .fns = ~ scales::percent(.x, accuracy = 0.01)
    )
  ) %>%
  mutate(category = factor(category, levels = order_levels)) %>%
  arrange(variable, category)

write.csv(
  demographics_table, "data_proc/demographics_table.csv", row.names = FALSE
)

# SEM Variables Distribution Plot ----
# Bug: 需要根据SEM实际用到的变量调整该图。
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

long_data_sem$label <- factor(long_data_sem$label, levels = var_info$label)

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

plotly::ggplotly(
  ggplot(long_data_sem) +
    geom_line(aes(year, prop, col = score, group = score)) +
    facet_wrap(.~ variable) +
    scale_color_brewer(palette = "RdYlGn", direction = 1, name = "Score")
)

long_data_sem %>%
  filter(score == 4 | score == 5) %>%
  group_by(variable, year) %>%
  summarise(prop = sum(prop), .groups = "drop") %>%
  mutate(prop = round(prop, digits = 2)) %>%
  pivot_wider(
    id_cols = variable, names_from = year, values_from = prop
  )

# Group Differences in Intention and Behavior ----
target_vars_diff <- c("wil_of_engage", "seper_recyc")
grouping_vars <- c("gender", "age_grp", "education_grp")

perform_group_comparison <- function(data, group_var, dep_var) {
  result_list <- lapply(unique(data$year), function(y) {
    df_year <- data %>%
      filter(year == y, !is.na(.data[[group_var]]), !is.na(.data[[dep_var]]))

    if (nrow(df_year) == 0) return(NULL)

    df_year[[group_var]] <- factor(df_year[[group_var]])
    n_levels <- length(levels(df_year[[group_var]]))

    if (n_levels < 2) return(NULL)

    test_res <- if (n_levels == 2) {
      wilcox.test(formula(paste(dep_var, "~", group_var)), data = df_year)
    } else {
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

calculate_group_stats <- function(data, group_var, dep_var) {
  data %>%
    filter(!is.na(.data[[group_var]]), !is.na(.data[[dep_var]])) %>%
    group_by(year, .data[[group_var]]) %>%
    summarise(
      mean_val = mean(.data[[dep_var]], na.rm = TRUE),
      median_val = median(.data[[dep_var]], na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    ungroup() %>%
    rename(group_level = as.name(group_var)) %>%
    mutate(
      group_variable = group_var,
      dependent_variable = dep_var
    ) %>%
    select(year, group_variable, dependent_variable, group_level, mean_val, median_val)
}

all_group_diff_results <- list()
all_comparison_results <- list()
all_stats_results <- list()
counter <- 1

for (gv in grouping_vars) {
  for (dv in target_vars_diff) {
    cat(paste0("Analyzing group differences for ", dv, " by ", gv, "...\n"))

    comp_res <- perform_group_comparison(ws_full, gv, dv)
    stats_res <- calculate_group_stats(ws_full, gv, dv)

    if (!is.null(comp_res) && nrow(comp_res) > 0) {
      all_comparison_results[[counter]] <- comp_res
    }
    all_stats_results[[counter]] <- stats_res

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

    overall_stats <- stats_res %>%
      group_by(group_variable, dependent_variable, group_level) %>%
      summarise(
        overall_mean = mean(mean_val, na.rm = TRUE),
        overall_median = median(median_val, na.rm = TRUE),
        .groups = "drop"
      )

    final_combined_res <- combined_yearly_results %>%
      left_join(overall_stats, by = c("group_variable", "dependent_variable", "group_level"))

    all_group_diff_results[[counter]] <- final_combined_res
    counter <- counter + 1
  }
}

final_group_diff_table <- bind_rows(all_group_diff_results) %>%
  arrange(group_variable, dependent_variable, year, group_level)
comparison_df <- bind_rows(all_comparison_results)
stats_df <- bind_rows(all_stats_results)

write.csv(final_group_diff_table, "data_proc/group_differences_table.csv", row.names = FALSE)
cat("Group differences table saved to data_proc/group_differences_table.csv\n")

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

p_mean_scores <- ggplot(mean_plot_data) +
  geom_line(
    aes(x = factor(year), y = mean_val, color = group_level_label, group = group_level_label),
    linewidth = 0.8
  ) +
  geom_point(
    aes(x = factor(year), y = mean_val, color = group_level_label)
  ) +
  geom_text(
    data = sig_data,
    aes(x = factor(year), y = 4.8, label = p_significance),
    size = 4, color = "#E64B35", fontface = "bold", inherit.aes = FALSE
  ) +
  facet_grid(group_var_label ~ dep_var_label) +
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

# SEM ----
## Variables ----
# ATT: 方案1 - 情感-认知态度
att_vars <- c("ws_attitude", "ws_interest", "threat")
# SN 拆分方案：
# INJ_NORM（指令性规范）= 制度/社会压力类
# DESC_NORM（描述性规范）= 参照他人行为类
inj_norm_vars  <- c("pr_atten", "regulate_law", "regulate_commu_rule")
desc_norm_vars <- c("if_neighbor_ws", "if_family_ws")
# 知觉行为控制PBC。
pbc_vars <- c("category_trouble", "time_cost_troub")
# 所有变量。
model_vars <- c(att_vars, inj_norm_vars, desc_norm_vars, pbc_vars)

## Reliability and Validity Analysis ----
# PLS-SEM uses reflective measurement models (Mode A), equivalent to CFA.
# Reliability: Cronbach's alpha + rho_c (composite reliability, CR).
# Convergent validity: AVE >= 0.5.
# Discriminant validity: HTMT < 0.85 (Henseler et al., 2015).

# Define the PLS measurement model (reflective constructs = Mode A).
# Single-item endogenous variables must also be declared here.
pls_mm <- constructs(
  reflective("ATT",       att_vars),
  reflective("INJ_NORM",  inj_norm_vars),
  reflective("DESC_NORM", desc_norm_vars),
  reflective("PBC",       pbc_vars),
  reflective("wil_of_engage", single_item("wil_of_engage")),
  reflective("seper_recyc",   single_item("seper_recyc"))
)

# Define the PLS structural model
# INJ_NORM、DESC_NORM、PBC → ATT → 意图 → 行为
# 移除所有不稳定的直接路径
pls_sm <- relationships(
  paths(from = c("INJ_NORM", "DESC_NORM", "PBC"), to = "ATT"),
  paths(from = "ATT",                             to = "wil_of_engage"),
  paths(from = "wil_of_engage",                   to = "seper_recyc")
)

# Function to calculate Cronbach's Alpha for each construct (unchanged)
get_alpha <- function(df, vars, label) {
  res <- psych::alpha(df[, vars], check.keys = TRUE)
  return(data.frame(Construct = label, Alpha = res$total$raw_alpha))
}

# Function to fit PLS model per year (no bootstrap for now)
fit_pls_year <- function(df_year, year_label) {
  cat(paste0("  Fitting PLS model for year: ", year_label, "\n"))

  # Convert all model variables to numeric and remove NAs (seminr requires complete cases)
  df_pls <- df_year %>%
    select(all_of(c(model_vars, "wil_of_engage", "seper_recyc"))) %>%
    mutate(across(everything(), as.numeric)) %>%
    na.omit() %>%
    as.data.frame()
  cat(paste0("    Complete cases: ", nrow(df_pls), "\n"))

  pls_fit <- estimate_pls(
    data              = df_pls,
    measurement_model = pls_mm,
    structural_model  = pls_sm,
    inner_weights     = path_weighting
  )

  list(pls_fit = pls_fit, boot_fit = NULL, year = year_label)
}

# Calculate Cronbach's alpha per year
reliability_results <- lapply(unique(ws_full$year), function(y) {
  df_year <- ws_full %>% filter(year == y)
  bind_rows(
    get_alpha(df_year, att_vars,       "ATT"),
    get_alpha(df_year, inj_norm_vars,  "INJ_NORM"),
    get_alpha(df_year, desc_norm_vars, "DESC_NORM"),
    get_alpha(df_year, pbc_vars,       "PBC")
  ) %>% mutate(year = y)
}) %>% 
  bind_rows()

print(reliability_results)

# Fit PLS per year and extract CR, AVE
year_levels <- levels(ws_full$year)
pls_results <- lapply(year_levels, function(y) {
  df_year <- ws_full %>% filter(year == y)
  fit_pls_year(df_year, y)
})
names(pls_results) <- year_levels

# Extract CR and AVE from each year's PLS summary
validity_results <- lapply(year_levels, function(y) {
  sm <- summary(pls_results[[y]]$pls_fit)
  # seminr summary$reliability has columns: alpha, rhoC (CR), AVE
  rel <- as.data.frame(sm$reliability)
  rel$Construct <- rownames(rel)
  rel$year <- y
  rel %>%
    select(year, Construct, CR = rhoC, AVE) %>%
    filter(Construct %in% c("ATT", "INJ_NORM", "DESC_NORM", "PBC"))
}) %>% bind_rows()

print("Construct Reliability (CR) and AVE:")
print(validity_results)

# Extract HTMT for discriminant validity
# 检验模型里的各个潜变量在统计上是不是两个真正独立、界限分明的概念。
htmt_results <- lapply(year_levels, function(y) {
  sm <- summary(pls_results[[y]]$pls_fit)
  htmt_mat <- as.data.frame(sm$validity$htmt)
  htmt_mat$Construct <- rownames(htmt_mat)
  htmt_mat$year <- y
  htmt_mat
}) %>% bind_rows()

print("HTMT (Discriminant Validity, threshold < 0.85):")
print(htmt_results)

write.csv(reliability_results, "data_proc/pls_reliability_results.csv", row.names = FALSE)
write.csv(validity_results,    "data_proc/pls_validity_results.csv",    row.names = FALSE)
write.csv(htmt_results,        "data_proc/pls_htmt_results.csv",        row.names = FALSE)
cat("Reliability and Validity results saved to data_proc/.\n")

## PLS-SEM Path Coefficients and Plot ----
# Extract path coefficients directly from pls_fit (no bootstrap, no p-values)
path_results <- lapply(year_levels, function(y) {
  sm <- summary(pls_results[[y]]$pls_fit)
  paths_df <- as.data.frame(sm$paths)

  # Row "R^2" and "AdjR^2" are summary rows; keep only coefficient rows
  coef_rows <- paths_df[!rownames(paths_df) %in% c("R^2", "AdjR^2"), , drop = FALSE]

  # Reshape: each column is an endogenous variable, each row is a predictor
  lapply(colnames(coef_rows), function(lhs_var) {
    col_vals <- coef_rows[[lhs_var]]
    non_zero <- which(!is.na(col_vals) & col_vals != 0)
    if (length(non_zero) == 0) return(NULL)
    tibble(
      year    = y,
      lhs     = lhs_var,
      rhs     = rownames(coef_rows)[non_zero],
      std.all = col_vals[non_zero]
    )
  }) %>% bind_rows()
}) %>% bind_rows()

cat("\nStandardized Path Coefficients (PLS-SEM, no bootstrap):\n")
print(path_results)

write.csv(
  path_results,
  "data_proc/pls_sem_path_coefficients.csv", row.names = FALSE
)
cat("PLS-SEM path coefficients saved to data_proc/pls_sem_path_coefficients.csv\n")

# Also extract R² per year
r2_results <- lapply(year_levels, function(y) {
  sm <- summary(pls_results[[y]]$pls_fit)
  r2_df <- as.data.frame(sm$paths)
  # paths summary includes R^2 for endogenous constructs
  r2_vals <- r2_df["R^2", , drop = FALSE]
  data.frame(
    year      = y,
    Construct = colnames(r2_vals),
    R2        = as.numeric(r2_vals)
  )
}) %>%
  bind_rows() %>%
  filter(!is.na(R2), R2 > 0)

print("R² for endogenous constructs:")
print(r2_results)
write.csv(r2_results, "data_proc/pls_r2_results.csv", row.names = FALSE)

# Plot path coefficient changes
path_colors <- c(
  "Injunctive Norm -> Attitude"   = "#4DBBD5",
  "Descriptive Norm -> Attitude"  = "#3C5488",
  "Perceived Control -> Attitude" = "#00A087",
  "Attitude -> Intention"         = "#4DBB15",
  "Intention -> Behavior"         = "#E64B35"
)

plot_data_pls_paths <- path_results %>%
  mutate(
    Path_full = case_when(
      lhs == "ATT"           & rhs == "INJ_NORM"      ~ "Injunctive Norm -> Attitude",
      lhs == "ATT"           & rhs == "DESC_NORM"     ~ "Descriptive Norm -> Attitude",
      lhs == "ATT"           & rhs == "PBC"           ~ "Perceived Control -> Attitude",
      lhs == "wil_of_engage" & rhs == "ATT"           ~ "Attitude -> Intention",
      lhs == "seper_recyc"   & rhs == "wil_of_engage" ~ "Intention -> Behavior",
      TRUE ~ "Other"
    ),
    year_num = as.numeric(year)
  ) %>%
  filter(Path_full != "Other") %>%
  mutate(
    Path_full = factor(Path_full, levels = c(
      "Injunctive Norm -> Attitude",
      "Descriptive Norm -> Attitude",
      "Perceived Control -> Attitude",
      "Attitude -> Intention",
      "Intention -> Behavior"
    ))
  )

# No p-values without bootstrap: all points filled (significance unknown)
p_pls_paths <- ggplot(plot_data_pls_paths, aes(x = year_num, y = std.all, color = Path_full)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3.5, stroke = 1) +
  facet_wrap(~ Path_full, ncol = 1) +
  scale_x_continuous(breaks = 2019:2023) +
  scale_color_manual(values = path_colors, guide = "none") +
  labs(
    x = "Year",
    y = "Standardized Coefficient (PLS-SEM, no bootstrap)"
  ) +
  theme_classic(base_size = 9) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 9, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 90),
    strip.background = element_rect(fill = "gray85", color = "gray50"),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.5)
  )

ggsave("data_proc/pls_model_path_plot.pdf", plot = p_pls_paths, width = 6, height = 10)
cat("PLS-SEM path coefficient plot saved to data_proc/pls_model_path_plot.pdf\n")

# ----------------------------------------------------------------------------
# 7. Spillover Effect Analysis (unchanged) ----
# ----------------------------------------------------------------------------

get_cor <- function(data, var_x, var_y) {
  result_list <- lapply(
    unique(data$year),
    function(y) {
      tar_df <- data %>%
        filter(year == y, !is.na(.data[[var_x]]), !is.na(.data[[var_y]]))

      if (nrow(tar_df) > 10) {
        val_x <- tar_df[[var_x]]
        val_y <- tar_df[[var_y]]

        unique_x <- unique(val_x[!is.na(val_x)])
        unique_y <- unique(val_y[!is.na(val_y)])
        is_binary_x <- length(unique_x) == 2
        is_binary_y <- length(unique_y) == 2

        if (is_binary_x || is_binary_y) {
          if (is_binary_x) {
            group_var <- val_x
            test_var  <- val_y
          } else {
            group_var <- val_y
            test_var  <- val_x
          }

          res_wilcox <- wilcox.test(test_var ~ group_var)
          p_val <- res_wilcox$p.value
          n1 <- sum(group_var == unique(group_var)[1])
          n2 <- sum(group_var == unique(group_var)[2])
          w_stat <- res_wilcox$statistic
          est <- (2 * w_stat / (n1 * n2)) - 1
          method_tag <- "Wilcoxon (Rank-Biserial r)"
        } else {
          res_stat   <- cor.test(val_x, val_y, method = "kendall")
          est        <- res_stat$estimate
          p_val      <- res_stat$p.value
          method_tag <- "Kendall's Tau"
        }

        res <- tibble(
          year = y, var_1 = var_x, var_2 = var_y,
          correlation_estimate = est, p_value = p_val, method = method_tag
        )
      } else {
        res <- tibble(
          year = y, var_1 = var_x, var_2 = var_y,
          correlation_estimate = NA_real_, p_value = NA_real_,
          method = "Insufficient data"
        )
      }
      return(res)
    }
  ) %>%
    bind_rows() %>%
    mutate(p_significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    ))
}

waste_sorting_vars  <- c("seper_recyc")
other_env_behaviors <- c("reuse_bag", "energy_concern", "save_energy",
                         "share_often", "freq_online_secondhand", "volun_expr")

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

final_spillover_table <- bind_rows(all_spillover_results) %>%
  arrange(var_1, var_2, year)

write.csv(final_spillover_table, "data_proc/spillover_correlation_table.csv", row.names = FALSE)
