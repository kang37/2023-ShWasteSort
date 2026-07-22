# ============================================================================
# 论文图表综合输出脚本
# 主模型：新替代模型（完整TPB，仅DESC_NORM，前因两两联系）
#   DESC_NORM → ATT, DESC_NORM → PBC, PBC → ATT
#   DESC_NORM / PBC / ATT → BI → BEH
#   PBC → BEH（直接路径）
# 溢出模型：以上述新模型为基准，叠加GreenBehav（2021-2023）
# 输出目录：data_proc/result_<YYYYMMDD_HHMMSS>/
# ============================================================================

pacman::p_load(
  dplyr, stringr, tidyr, patchwork, ggplot2, readxl, purrr, showtext,
  psych, seminr, ggh4x
)
showtext::showtext_auto()

out_dir <- file.path("data_proc", format(Sys.time(), "result_%Y%m%d"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
cat("Output directory:", out_dir, "\n\n")

# ============================================================================
# 1. 数据加载
# ============================================================================
colname_mapping <- read_excel("data_raw/colname_mapping.xlsx")

create_rename_vector <- function(mapping_df, year) {
  col_year <- paste0("col_", year)
  mapping_year <- mapping_df %>%
    filter(!is.na(.data[[col_year]])) %>%
    select(old_name = all_of(col_year), new_name = unified_name_en)
  setNames(mapping_year$old_name, mapping_year$new_name)
}

vars_needed <- c(
  "year",
  "ws_attitude", "ws_interest", "threat",
  "if_neighbor_ws", "if_family_ws",
  "category_trouble", "time_cost_troub",
  "wil_of_engage", "seper_recyc",
  "reuse_bag", "energy_concern", "save_energy",
  "gender", "age", "education", "occupation",
  "income", "family_size", "elder_num",
  "hukou", "residence_area", "residence_commu"
)

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
  bind_rows() %>%
  mutate(
    across(
      .cols = any_of(
        colname_mapping %>%
          filter(var_scale5_rev == 1) %>%
          pull(unified_name_en)
      ),
      .fns = ~ { x <- as.numeric(.x); 5 + 1 - x }
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

cat("Data loaded. N by year:\n")
print(table(ws_full$year))

# ============================================================================
# 2. 人口统计表
# ============================================================================
cat("\n--- Demographics ---\n")

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
      "4" ~ "foreign_enterprise", "5" ~ "private_enterprise",
      "6" ~ "self_employed", "7" ~ "freelancer", "8" ~ "retired",
      "9" ~ "student", "10" ~ "occ_other"
    ),
    income = case_match(
      as.character(income),
      "1" ~ "≤ 3", "2" ~ "3~5", "3" ~ "5~10", "4" ~ "10~15",
      "5" ~ "15~20", "6" ~ "20~30", "7" ~ "> 30"
    )
  )

order_levels <- c(
  "male", "female",
  "primary", "junior_high", "senior_high", "college_or_higher", "edu_other",
  "government", "institution", "state_enterprise", "foreign_enterprise",
  "private_enterprise", "self_employed", "freelancer", "retired", "student", "occ_other",
  "≤ 3", "3~5", "5~10", "10~15", "15~20", "20~30", "> 30"
)

gene_des_proc <- ws_full_text %>%
  pivot_longer(cols = c(gender, education, occupation, income),
               names_to = "variable", values_to = "category") %>%
  count(year, variable, category, name = "count") %>%
  group_by(year, variable) %>%
  mutate(percentage = count / sum(count),
         label = scales::percent(percentage, accuracy = 2)) %>%
  ungroup()

gene_des_avg <- gene_des_proc %>%
  group_by(variable, category) %>%
  summarise(percentage = mean(percentage),
            label = scales::percent(percentage, accuracy = 2),
            .groups = "drop")

demographics_table <- bind_rows(
  gene_des_proc, gene_des_avg %>% mutate(year = "Average")
) %>%
  select(variable, category, year, percentage) %>%
  pivot_wider(names_from = year, values_from = percentage) %>%
  mutate(across(where(is.numeric), ~ scales::percent(.x, accuracy = 0.01))) %>%
  mutate(category = factor(category, levels = order_levels)) %>%
  arrange(variable, category)

write.csv(demographics_table, file.path(out_dir, "demographics_table.csv"), row.names = FALSE)
cat("Saved: demographics_table.csv\n")

# ============================================================================
# 2b. 图：变量值分布（各年份李克特量表1-5分占比）
# ============================================================================
dist_vars <- c(
  "ws_attitude", "ws_interest", "threat",
  "if_neighbor_ws", "if_family_ws",
  "category_trouble", "time_cost_troub",
  "wil_of_engage", "seper_recyc",
  "reuse_bag", "energy_concern", "save_energy"
)

var_labels <- c(
  "ws_attitude"      = "ATT: ws_attitude",
  "ws_interest"      = "ATT: ws_interest",
  "threat"           = "ATT: threat",
  "if_neighbor_ws"   = "DESC_NORM: if_neighbor_ws",
  "if_family_ws"     = "DESC_NORM: if_family_ws",
  "category_trouble" = "PBC: category_trouble",
  "time_cost_troub"  = "PBC: time_cost_troub",
  "wil_of_engage"    = "BI: wil_of_engage",
  "seper_recyc"      = "BEH: seper_recyc",
  "reuse_bag"        = "GreenBehav: reuse_bag",
  "energy_concern"   = "GreenBehav: energy_concern",
  "save_energy"      = "GreenBehav: save_energy"
)

dist_data <- ws_full %>%
  select(year, all_of(dist_vars)) %>%
  pivot_longer(cols = all_of(dist_vars), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value), value %in% 1:5) %>%
  mutate(
    value    = factor(value, levels = 1:5),
    var_label = factor(var_labels[variable], levels = var_labels)
  ) %>%
  count(year, var_label, value) %>%
  group_by(year, var_label) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

likert_colors <- c(
  "1" = "#D73027",
  "2" = "#FC8D59",
  "3" = "#FEE090",
  "4" = "#91BFDB",
  "5" = "#4575B4"
)

p_dist <- ggplot(dist_data, aes(x = year, y = prop, fill = value)) +
  geom_col(width = 0.7, position = "stack") +
  scale_fill_manual(
    values = likert_colors,
    name   = "Score",
    labels = c("1 (Strongly\nDisagree)", "2", "3", "4", "5 (Strongly\nAgree)")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  facet_wrap(~ var_label, ncol = 3) +
  labs(x = "Year", y = "Proportion") +
  theme_bw(base_size = 11) +
  theme(
    strip.text       = element_text(size = 8),
    axis.text.x      = element_text(size = 8),
    legend.position  = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave(
  file.path(out_dir, "var_distribution_by_year.pdf"),
  p_dist, width = 10, height = 12
)
cat("Saved: var_distribution_by_year.pdf\n")

dist_table <- dist_data %>%
  mutate(prop = round(prop * 100, 1)) %>%
  select(variable = var_label, year, score = value, percent = prop) %>%
  pivot_wider(names_from = score, values_from = percent,
              names_prefix = "score_") %>%
  arrange(variable, year)
write.csv(dist_table, file.path(out_dir, "var_distribution_by_year.csv"), row.names = FALSE)
cat("Saved: var_distribution_by_year.csv\n")

# ============================================================================
# 3. 组间差异（意图与行为）
# ============================================================================
cat("\n--- Group Differences ---\n")

perform_group_comparison <- function(data, group_var, dep_var) {
  lapply(unique(data$year), function(y) {
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
    tibble(year = y, group_variable = group_var, dependent_variable = dep_var,
           p_value = test_res$p.value,
           p_significance = case_when(
             test_res$p.value < 0.001 ~ "***", test_res$p.value < 0.01 ~ "**",
             test_res$p.value < 0.05 ~ "*", TRUE ~ ""
           ),
           test_type = ifelse(n_levels == 2, "Wilcoxon", "Kruskal-Wallis"))
  }) %>% bind_rows()
}

calculate_group_stats <- function(data, group_var, dep_var) {
  data %>%
    filter(!is.na(.data[[group_var]]), !is.na(.data[[dep_var]])) %>%
    group_by(year, .data[[group_var]]) %>%
    summarise(mean_val = mean(.data[[dep_var]], na.rm = TRUE),
              median_val = median(.data[[dep_var]], na.rm = TRUE),
              .groups = "drop") %>%
    rename(group_level = as.name(group_var)) %>%
    mutate(group_variable = group_var, dependent_variable = dep_var)
}

grouping_vars    <- c("gender", "age_grp", "education_grp")
target_vars_diff <- c("wil_of_engage", "seper_recyc")

all_comp <- list(); all_stats <- list(); counter <- 1
for (gv in grouping_vars) {
  for (dv in target_vars_diff) {
    all_comp[[counter]]  <- perform_group_comparison(ws_full, gv, dv)
    all_stats[[counter]] <- calculate_group_stats(ws_full, gv, dv)
    counter <- counter + 1
  }
}

comparison_df <- bind_rows(all_comp)
stats_df      <- bind_rows(all_stats)

group_diff_table <- left_join(
  stats_df,
  comparison_df %>% select(year, group_variable, dependent_variable,
                            p_value, p_significance, test_type),
  by = c("year", "group_variable", "dependent_variable")
) %>%
  arrange(group_variable, dependent_variable, year, group_level)

write.csv(group_diff_table, file.path(out_dir, "group_differences_table.csv"), row.names = FALSE)

mean_plot_data <- stats_df %>%
  mutate(
    group_var_label = case_when(
      group_variable == "gender"        ~ "Gender",
      group_variable == "age_grp"       ~ "Age Group",
      group_variable == "education_grp" ~ "Education Level",
      TRUE ~ group_variable
    ),
    dep_var_label = case_when(
      dependent_variable == "wil_of_engage" ~ "Intention",
      dependent_variable == "seper_recyc"   ~ "Behavior",
      TRUE ~ dependent_variable
    ),
    group_level_label = case_when(
      group_level == "1"            ~ "Male",
      group_level == "2"            ~ "Female",
      group_level == "<=25"         ~ "<=25",
      group_level == "25~40"        ~ "25~40",
      group_level == "40_60"        ~ "40~60",
      group_level == ">60"          ~ ">60",
      group_level == "under_senior" ~ "High School or Below",
      group_level == "undergrad"    ~ "Undergraduate",
      group_level == "beyond_master"~ "Graduate or Above",
      TRUE ~ as.character(group_level)
    )
  )

sig_data <- comparison_df %>%
  mutate(
    group_var_label = case_when(
      group_variable == "gender"        ~ "Gender",
      group_variable == "age_grp"       ~ "Age Group",
      group_variable == "education_grp" ~ "Education Level",
      TRUE ~ group_variable
    ),
    dep_var_label = case_when(
      dependent_variable == "wil_of_engage" ~ "Intention",
      dependent_variable == "seper_recyc"   ~ "Behavior",
      TRUE ~ dependent_variable
    ),
    significant = p_value < 0.05
  )

write.csv(mean_plot_data, file.path(out_dir, "mean_intension_behavior_of_group.csv"), row.names = FALSE)
write.csv(sig_data,       file.path(out_dir, "mean_intension_behavior_of_group_statistic.csv"), row.names = FALSE)
cat("Saved: group_differences_table.csv + group mean files\n")

# ============================================================================
# 4. 新替代模型：变量与PLS定义
# ============================================================================
att_vars       <- c("ws_attitude", "ws_interest", "threat")
desc_norm_vars <- c("if_neighbor_ws", "if_family_ws")
pbc_vars       <- c("category_trouble", "time_cost_troub")
model_vars     <- c(att_vars, desc_norm_vars, pbc_vars)
spill_vars     <- c("reuse_bag", "energy_concern", "save_energy")
spill_years    <- c("2021", "2022", "2023")

N_BOOT      <- 1000
year_levels <- levels(ws_full$year)

sig_star <- function(p) case_when(
  p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ ""
)

# 测量模型（主模型，不含GreenBehav）
pls_mm <- constructs(
  reflective("ATT",           att_vars),
  reflective("DESC_NORM",     desc_norm_vars),
  reflective("PBC",           pbc_vars),
  reflective("wil_of_engage", single_item("wil_of_engage")),
  reflective("seper_recyc",   single_item("seper_recyc"))
)

# 结构模型：完整TPB（DESC_NORM替代SN）+ 前因两两联系 + PBC→BEH
pls_sm <- relationships(
  paths(from = c("DESC_NORM", "PBC", "ATT"), to = "wil_of_engage"),
  paths(from = "wil_of_engage", to = "seper_recyc"),
  paths(from = "PBC",           to = "seper_recyc"),
  paths(from = "DESC_NORM",     to = "ATT"),
  paths(from = "DESC_NORM",     to = "PBC"),
  paths(from = "PBC",           to = "ATT")
)

# ============================================================================
# 5. 可靠性（Cronbach's alpha）
# ============================================================================
get_alpha <- function(df, vars, label) {
  res <- psych::alpha(df[, vars], check.keys = TRUE)
  data.frame(Construct = label, Alpha = res$total$raw_alpha)
}

reliability_results <- lapply(year_levels, function(y) {
  df_year <- ws_full %>% filter(year == y)
  bind_rows(
    get_alpha(df_year, att_vars,       "ATT"),
    get_alpha(df_year, desc_norm_vars, "DESC_NORM"),
    get_alpha(df_year, pbc_vars,       "PBC")
  ) %>% mutate(year = y)
}) %>% bind_rows()

print(reliability_results)

reliability_wide <- reliability_results %>%
  mutate(Metric = "Alpha") %>%
  pivot_wider(names_from = year, values_from = Alpha) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  select(Metric, Construct, everything())

# ============================================================================
# 6. 逐年拟合PLS + Bootstrap
# ============================================================================
cat("\n--- Fitting PLS models ---\n")

fit_pls_year <- function(df_year, year_label) {
  cat(paste0("  Year: ", year_label, "\n"))
  df_pls <- df_year %>%
    select(all_of(c(model_vars, "wil_of_engage", "seper_recyc"))) %>%
    mutate(across(everything(), as.numeric)) %>%
    na.omit() %>% as.data.frame()
  cat(paste0("    n = ", nrow(df_pls), "\n"))
  pls_fit  <- estimate_pls(df_pls, pls_mm, pls_sm, inner_weights = path_weighting)
  boot_fit <- bootstrap_model(pls_fit, nboot = N_BOOT, seed = 42)
  list(pls_fit = pls_fit, boot_fit = boot_fit, df_pls = df_pls, year = year_label)
}

pls_results <- lapply(year_levels, function(y) {
  fit_pls_year(ws_full %>% filter(year == y), y)
})
names(pls_results) <- year_levels

# ============================================================================
# 7. CR / AVE / HTMT
# ============================================================================
validity_results <- lapply(year_levels, function(y) {
  sm  <- summary(pls_results[[y]]$pls_fit)
  rel <- as.data.frame(sm$reliability)
  rel$Construct <- rownames(rel)
  rel %>% mutate(year = y) %>%
    select(year, Construct, CR = rhoC, AVE) %>%
    filter(Construct %in% c("ATT", "DESC_NORM", "PBC"))
}) %>% bind_rows()

htmt_results <- lapply(year_levels, function(y) {
  sm       <- summary(pls_results[[y]]$pls_fit)
  htmt_mat <- as.data.frame(sm$validity$htmt)
  htmt_mat$Construct <- rownames(htmt_mat)
  htmt_mat$year      <- y
  htmt_mat
}) %>% bind_rows()

print("CR and AVE:"); print(as.data.frame(validity_results))
print("HTMT:");       print(as.data.frame(htmt_results))

write.csv(htmt_results, file.path(out_dir, "pls_htmt_results.csv"), row.names = FALSE)

validity_wide <- validity_results %>%
  pivot_longer(cols = c(CR, AVE), names_to = "Metric", values_to = "value") %>%
  mutate(value = round(value, 3)) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  select(Metric, Construct, everything()) %>%
  arrange(Metric, Construct)

pls_measurement_wide <- bind_rows(reliability_wide, validity_wide) %>%
  arrange(Construct, Metric)
write.csv(pls_measurement_wide, file.path(out_dir, "pls_measurement_results_wide.csv"), row.names = FALSE)
cat("Saved: pls_measurement_results_wide.csv\n")

# ============================================================================
# 8. 路径系数（Bootstrap）
# ============================================================================
path_results <- lapply(year_levels, function(y) {
  sm <- summary(pls_results[[y]]$boot_fit)
  bp <- as.data.frame(sm$bootstrapped_paths)
  bp$path_raw <- rownames(bp)
  bp %>% transmute(
    year    = y,
    rhs     = trimws(sub("\\s+->\\s+.*", "", path_raw)),
    lhs     = trimws(sub(".*->\\s+",     "", path_raw)),
    beta    = `Bootstrap Mean`,
    se      = `Bootstrap SD`,
    ci_low  = `2.5% CI`,
    ci_high = `97.5% CI`,
    p_value = `Bootstrap P Val`,
    sig     = sig_star(p_value)
  )
}) %>% bind_rows()

cat("\nPath coefficients:\n")
print(as.data.frame(path_results))
write.csv(path_results, file.path(out_dir, "pls_sem_path_coefficients.csv"), row.names = FALSE)

# ============================================================================
# 9. R²
# ============================================================================
r2_results <- lapply(year_levels, function(y) {
  sm      <- summary(pls_results[[y]]$pls_fit)
  r2_df   <- as.data.frame(sm$paths)
  r2_vals <- r2_df["R^2", , drop = FALSE]
  data.frame(year = y, Construct = colnames(r2_vals), R2 = as.numeric(r2_vals))
}) %>% bind_rows() %>% filter(!is.na(R2), R2 > 0)

print("R²:"); print(r2_results)
write.csv(r2_results, file.path(out_dir, "pls_r2_results.csv"), row.names = FALSE)

# ============================================================================
# 10. MGA：所有年份两两比较
# ============================================================================
cat("\n--- MGA ---\n")
all_pairs <- combn(year_levels, 2, simplify = FALSE)

mga_results <- lapply(all_pairs, function(pair) {
  ya <- pair[1]; yb <- pair[2]
  cat(paste0("  MGA: ", ya, " vs ", yb, "\n"))
  df_combined <- bind_rows(pls_results[[ya]]$df_pls, pls_results[[yb]]$df_pls)
  pls_combined <- estimate_pls(df_combined, pls_mm, pls_sm, inner_weights = path_weighting)
  condition <- c(rep(TRUE, nrow(pls_results[[ya]]$df_pls)),
                 rep(FALSE, nrow(pls_results[[yb]]$df_pls)))
  tryCatch({
    mga <- estimate_pls_mga(pls_combined, condition, nboot = N_BOOT)
    mga$year_a <- ya; mga$year_b <- yb
    mga$sig    <- sig_star(mga$pls_mga_p)
    as.data.frame(mga)
  }, error = function(e) { cat("    Error:", conditionMessage(e), "\n"); NULL })
}) %>% bind_rows()

print(as.data.frame(mga_results))
write.csv(mga_results, file.path(out_dir, "pls_mga_all_pairs.csv"), row.names = FALSE)

# ============================================================================
# 11. 图：路径系数年份趋势（纵轴统一比例尺）
# 各面板纵轴范围独立，但1单位所对应的物理高度一致（ggh4x::force_panelsizes）
# ============================================================================
path_labels <- c(
  "DESC_NORM->wil_of_engage"   = "Desc. Norm -> Intention",
  "PBC->wil_of_engage"         = "PBC -> Intention",
  "ATT->wil_of_engage"         = "Attitude -> Intention",
  "wil_of_engage->seper_recyc" = "Intention -> Behavior",
  "PBC->seper_recyc"           = "PBC -> Behavior (direct)",
  "DESC_NORM->ATT"             = "Desc. Norm -> Attitude",
  "DESC_NORM->PBC"             = "Desc. Norm -> PBC",
  "PBC->ATT"                   = "PBC -> Attitude"
)

path_colors <- c(
  "Desc. Norm -> Intention"    = "#1B7837",  # 深绿
  "PBC -> Intention"           = "#5AAE61",  # 中绿
  "Attitude -> Intention"      = "#A6D96A",  # 浅绿
  "Intention -> Behavior"      = "#D73027",  # 红
  "PBC -> Behavior (direct)"   = "#F46D43",  # 橙
  "Desc. Norm -> Attitude"     = "#2166AC",  # 深蓝
  "Desc. Norm -> PBC"          = "#4393C3",  # 中蓝
  "PBC -> Attitude"            = "#92C5DE"   # 浅蓝
)

plot_data <- path_results %>%
  mutate(
    path_key  = paste0(rhs, "->", lhs),
    Path_full = path_labels[path_key],
    year_num  = as.numeric(year),
    sig_shape = p_value < 0.05
  ) %>%
  filter(!is.na(Path_full)) %>%
  mutate(
    Path_full  = factor(Path_full, levels = names(path_colors)),
    point_fill = if_else(sig_shape, Path_full, factor("ns"))
  )

# 各路径y轴范围：基于CI上下界 + 10%边距
path_ranges <- plot_data %>%
  group_by(Path_full) %>%
  summarise(
    ylo_data = min(ci_low,  na.rm = TRUE),
    yhi_data = max(ci_high, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    pad  = (yhi_data - ylo_data) * 0.10,
    ylo  = ylo_data - pad,
    yhi  = yhi_data + pad,
    span = yhi - ylo
  )

# 面板高度与y轴跨度成正比 → 统一比例尺（HEIGHT_UNIT英寸/单位）
HEIGHT_UNIT   <- 1.6
path_order    <- levels(plot_data$Path_full)
ranges_ord    <- path_ranges[match(path_order, path_ranges$Path_full), ]
panel_heights <- ranges_ord$span * HEIGHT_UNIT

# 每个面板独立的y轴刻度（由范围自动确定步距）
y_scales <- lapply(seq_along(path_order), function(i) {
  r   <- ranges_ord[i, ]
  brk <- pretty(c(r$ylo, r$yhi), n = if (r$span > 0.6) 5 else 4)
  brk <- brk[brk >= r$ylo & brk <= r$yhi]
  scale_y_continuous(limits = c(r$ylo, r$yhi), breaks = brk)
})

fill_values <- c(setNames(unname(path_colors), names(path_colors)), ns = "white")

# 路径分组（三列）
col_blue  <- c("Desc. Norm -> Attitude", "Desc. Norm -> PBC", "PBC -> Attitude")
col_green <- c("Desc. Norm -> Intention", "PBC -> Intention", "Attitude -> Intention")
col_red   <- c("Intention -> Behavior", "PBC -> Behavior (direct)")

make_path_col <- function(paths_in_col, data, ranges, HEIGHT_UNIT, fill_values, x_breaks) {
  d <- data %>% filter(Path_full %in% paths_in_col) %>%
    mutate(Path_full = factor(Path_full, levels = paths_in_col))
  r <- ranges %>% filter(Path_full %in% paths_in_col) %>%
    slice(match(paths_in_col, Path_full))
  ph <- r$span * HEIGHT_UNIT
  ys <- lapply(seq_along(paths_in_col), function(i) {
    ri  <- r[i, ]
    brk <- pretty(c(ri$ylo, ri$yhi), n = if (ri$span > 0.6) 5 else 4)
    brk <- brk[brk >= ri$ylo & brk <= ri$yhi]
    scale_y_continuous(limits = c(ri$ylo, ri$yhi), breaks = brk)
  })
  p <- ggplot(d, aes(x = year_num, y = beta, color = Path_full)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = Path_full),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(aes(fill = point_fill), shape = 21, size = 3.5, stroke = 1) +
    geom_text(aes(label = sig), vjust = -1.0, size = 3, show.legend = FALSE) +
    facet_wrap(~ Path_full, ncol = 1, scales = "free_y") +
    ggh4x::facetted_pos_scales(y = ys) +
    ggh4x::force_panelsizes(rows = unit(ph, "in")) +
    scale_x_continuous(breaks = x_breaks) +
    scale_color_manual(values = path_colors, guide = "none") +
    scale_fill_manual(values = fill_values, guide = "none") +
    labs(x = "Year", y = "Standardized Coefficient") +
    theme_classic(base_size = 9) +
    theme(
      axis.text.x      = element_text(angle = 90),
      strip.background = element_rect(fill = "gray85", color = "gray50"),
      panel.border     = element_rect(color = "gray50", fill = NA, linewidth = 0.5)
    )
  list(plot = p, height = sum(ph))
}

col1 <- make_path_col(col_blue,  plot_data, ranges_ord, HEIGHT_UNIT, fill_values, 2019:2023)
col2 <- make_path_col(col_green, plot_data, ranges_ord, HEIGHT_UNIT, fill_values, 2019:2023)
col3 <- make_path_col(col_red,   plot_data, ranges_ord, HEIGHT_UNIT, fill_values, 2019:2023)

p_paths <- col1$plot + col2$plot + col3$plot +
  patchwork::plot_layout(ncol = 3) +
  patchwork::plot_annotation(
    title    = "Alt Model: DESC_NORM / PBC / ATT fully connected + direct to BEH",
    subtitle = paste0("Bootstrap n = ", N_BOOT,
                      "  |  filled = p < .05, white = n.s.  |  Consistent y-axis scale")
  )

total_height <- max(col1$height, col2$height, col3$height) + 2.5
ggsave(file.path(out_dir, "pls_model_path_plot.pdf"), plot = p_paths,
       width = 12, height = total_height)
cat("Saved: pls_model_path_plot.pdf\n")

# ============================================================================
# 12. 图：MGA 热力图
# ============================================================================
heatmap_data <- mga_results %>%
  mutate(
    path_key   = paste0(source, "->", target),
    path_label = path_labels[path_key],
    sig_level  = case_when(
      pls_mga_p < .001 ~ "p < .001",
      pls_mga_p < .01  ~ "p < .01",
      pls_mga_p < .05  ~ "p < .05",
      TRUE             ~ "n.s."
    ),
    sig_level = factor(sig_level, levels = c("p < .001", "p < .01", "p < .05", "n.s."))
  ) %>%
  filter(!is.na(path_label)) %>%
  { bind_rows(., rename(., year_a = year_b, year_b = year_a)) } %>%
  filter(year_a != year_b) %>%
  mutate(path_label = factor(path_label, levels = rev(names(path_colors))))

p_heatmap <- ggplot(heatmap_data, aes(x = year_b, y = year_a, fill = sig_level)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(
    aes(label = ifelse(sig_level == "n.s.", "", as.character(sig_level))),
    color = "white", fontface = "bold", size = 3
  ) +
  facet_wrap(~ path_label, ncol = 2) +
  scale_fill_manual(
    values = c("p < .001" = "#C0392B", "p < .01" = "#E74C3C",
               "p < .05" = "#F1948A", "n.s." = "gray92"),
    name = "MGA significance"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "PLS-MGA: Path Coefficient Differences Between Years",
    subtitle = paste0("All C(5,2)=10 pairs  |  bootstrap n = ", N_BOOT,
                      "  |  red = significant difference"),
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 10)

ggsave(file.path(out_dir, "pls_mga_heatmap.pdf"), p_heatmap, width = 14, height = 14)
ggsave(file.path(out_dir, "pls_mga_heatmap.png"), p_heatmap, width = 14, height = 14, dpi = 180)
cat("Saved: pls_mga_heatmap.pdf / .png\n")

# ============================================================================
# 13. 溢出分析（以新替代模型为基准）
# ============================================================================
cat("\n--- Spillover Analysis ---\n")

# 溢出变量可用性检验
cat("Spillover variable availability by year:\n")
ws_full %>%
  group_by(year) %>%
  summarise(across(all_of(spill_vars), ~ sum(!is.na(.))), .groups = "drop") %>%
  print()

# 辅助：构建PLS数据框
make_pls_df <- function(data, year_val, extra_vars = character(0)) {
  all_v <- c(model_vars, "wil_of_engage", "seper_recyc", extra_vars)
  data %>%
    filter(year == year_val) %>%
    select(all_of(intersect(all_v, names(data)))) %>%
    mutate(across(everything(), as.numeric)) %>%
    na.omit() %>% as.data.frame()
}

# 辅助：提取bootstrap路径
extract_paths_boot <- function(boot_sum, year_val, model_tag) {
  bp <- as.data.frame(boot_sum$bootstrapped_paths)
  bp$path_label <- rownames(bp)
  bp %>% transmute(
    year    = year_val, model = model_tag, path = path_label,
    beta    = `Bootstrap Mean`, se = `Bootstrap SD`,
    ci_low  = `2.5% CI`, ci_high = `97.5% CI`,
    p_value = `Bootstrap P Val`
  )
}

# 测量模型（含GreenBehav复合构念）
pls_mm_spill <- constructs(
  reflective("ATT",           att_vars),
  reflective("DESC_NORM",     desc_norm_vars),
  reflective("PBC",           pbc_vars),
  composite("GreenBehav",     spill_vars, weights = correlation_weights),
  reflective("wil_of_engage", single_item("wil_of_engage")),
  reflective("seper_recyc",   single_item("seper_recyc"))
)

# 各溢出模型结构方程（均在新替代模型基础上叠加GreenBehav路径）
pls_sm_base_spill <- relationships(   # M_base：仅新替代模型，无GreenBehav
  paths(from = c("DESC_NORM", "PBC", "ATT"), to = "wil_of_engage"),
  paths(from = "wil_of_engage", to = "seper_recyc"),
  paths(from = "PBC",           to = "seper_recyc"),
  paths(from = "DESC_NORM",     to = "ATT"),
  paths(from = "DESC_NORM",     to = "PBC"),
  paths(from = "PBC",           to = "ATT")
)
pls_mm_base_spill <- constructs(   # M_base 对应测量模型（无GreenBehav）
  reflective("ATT",           att_vars),
  reflective("DESC_NORM",     desc_norm_vars),
  reflective("PBC",           pbc_vars),
  reflective("wil_of_engage", single_item("wil_of_engage")),
  reflective("seper_recyc",   single_item("seper_recyc"))
)

pls_sm_bi <- relationships(         # M_spill_bi：GreenBehav → BI（经意图）
  paths(from = c("DESC_NORM", "PBC", "ATT"), to = "wil_of_engage"),
  paths(from = "wil_of_engage", to = "seper_recyc"),
  paths(from = "PBC",           to = "seper_recyc"),
  paths(from = "DESC_NORM",     to = "ATT"),
  paths(from = "DESC_NORM",     to = "PBC"),
  paths(from = "PBC",           to = "ATT"),
  paths(from = "GreenBehav",    to = "wil_of_engage")
)

pls_sm_direct <- relationships(     # M_spill_direct：GreenBehav → BEH（直接）
  paths(from = c("DESC_NORM", "PBC", "ATT"), to = "wil_of_engage"),
  paths(from = "wil_of_engage", to = "seper_recyc"),
  paths(from = "PBC",           to = "seper_recyc"),
  paths(from = "DESC_NORM",     to = "ATT"),
  paths(from = "DESC_NORM",     to = "PBC"),
  paths(from = "PBC",           to = "ATT"),
  paths(from = "GreenBehav",    to = "seper_recyc")
)

pls_sm_full <- relationships(       # M_spill_full：GreenBehav → BI + BEH（同时）
  paths(from = c("DESC_NORM", "PBC", "ATT"), to = "wil_of_engage"),
  paths(from = "wil_of_engage", to = "seper_recyc"),
  paths(from = "PBC",           to = "seper_recyc"),
  paths(from = "DESC_NORM",     to = "ATT"),
  paths(from = "DESC_NORM",     to = "PBC"),
  paths(from = "PBC",           to = "ATT"),
  paths(from = "GreenBehav",    to = "wil_of_engage"),
  paths(from = "GreenBehav",    to = "seper_recyc")
)

## M_base（2021-2023，无GreenBehav）
cat("Fitting M_base (2021-2023)...\n")
base_results <- lapply(spill_years, function(y) {
  df <- make_pls_df(ws_full, y)
  cat("  Year:", y, "n =", nrow(df), "\n")
  fit  <- estimate_pls(df, pls_mm_base_spill, pls_sm_base_spill, inner_weights = path_weighting)
  boot <- bootstrap_model(fit, nboot = N_BOOT, seed = 42)
  list(fit = fit, boot = boot, year = y)
})
names(base_results) <- spill_years
base_paths <- lapply(spill_years, function(y)
  extract_paths_boot(summary(base_results[[y]]$boot), y, "M_base")
) %>% bind_rows() %>% mutate(sig = sig_star(p_value))

## M_spill_bi（GreenBehav → BI）
cat("Fitting M_spill_bi...\n")
bi_results <- lapply(spill_years, function(y) {
  df <- make_pls_df(ws_full, y, extra_vars = spill_vars)
  fit  <- estimate_pls(df, pls_mm_spill, pls_sm_bi, inner_weights = path_weighting)
  boot <- bootstrap_model(fit, nboot = N_BOOT, seed = 42)
  list(fit = fit, boot = boot, year = y)
})
names(bi_results) <- spill_years
bi_paths <- lapply(spill_years, function(y)
  extract_paths_boot(summary(bi_results[[y]]$boot), y, "M_spill_bi")
) %>% bind_rows() %>% mutate(sig = sig_star(p_value))

## M_spill_direct（GreenBehav → BEH）
cat("Fitting M_spill_direct...\n")
direct_results <- lapply(spill_years, function(y) {
  df <- make_pls_df(ws_full, y, extra_vars = spill_vars)
  fit  <- estimate_pls(df, pls_mm_spill, pls_sm_direct, inner_weights = path_weighting)
  boot <- bootstrap_model(fit, nboot = N_BOOT, seed = 42)
  list(fit = fit, boot = boot, year = y)
})
names(direct_results) <- spill_years
direct_paths <- lapply(spill_years, function(y)
  extract_paths_boot(summary(direct_results[[y]]$boot), y, "M_spill_direct")
) %>% bind_rows() %>% mutate(sig = sig_star(p_value))

## M_spill_full（GreenBehav → BI + BEH）
cat("Fitting M_spill_full...\n")
full_results <- lapply(spill_years, function(y) {
  df <- make_pls_df(ws_full, y, extra_vars = spill_vars)
  fit  <- estimate_pls(df, pls_mm_spill, pls_sm_full, inner_weights = path_weighting)
  boot <- bootstrap_model(fit, nboot = N_BOOT, seed = 42)
  list(fit = fit, boot = boot, year = y)
})
names(full_results) <- spill_years
full_paths <- lapply(spill_years, function(y)
  extract_paths_boot(summary(full_results[[y]]$boot), y, "M_spill_full")
) %>% bind_rows() %>% mutate(sig = sig_star(p_value))

## 中介效应：GreenBehav → BI → BEH（经由意图的间接效应，来自M_spill_full）
parse_ie <- function(ie_mat, year_val, path_label) {
  if (is.null(ie_mat)) return(NULL)
  tibble(year = year_val, path = path_label,
         beta    = ie_mat[1, "Bootstrap Mean"],
         se      = ie_mat[1, "Bootstrap SD"],
         ci_low  = ie_mat[1, "2.5% CI"],
         ci_high = ie_mat[1, "97.5% CI"],
         p_value = ie_mat[1, "Bootstrap P Val"])
}

mediation_results <- lapply(spill_years, function(y) {
  boot <- full_results[[y]]$boot
  ie <- tryCatch(
    specific_effect_significance(boot, from = "GreenBehav",
      through = "wil_of_engage", to = "seper_recyc", alpha = 0.05),
    error = function(e) NULL
  )
  parse_ie(ie, y, "GreenBehav -> BI -> BEH (indirect)")
}) %>% bind_rows() %>% mutate(sig = sig_star(p_value))

cat("\nIndirect effects:\n")
print(as.data.frame(mediation_results))

## Layer 1 相关
get_cor_kendall <- function(data, var_x, var_y) {
  lapply(levels(data$year), function(y) {
    df <- data %>% filter(year == y, !is.na(.data[[var_x]]), !is.na(.data[[var_y]]))
    if (nrow(df) < 10)
      return(tibble(year = y, var_from = var_x, var_to = var_y,
                    tau = NA_real_, p_value = NA_real_, n = nrow(df)))
    ct <- cor.test(df[[var_x]], df[[var_y]], method = "kendall")
    tibble(year = y, var_from = var_x, var_to = var_y,
           tau = ct$estimate, p_value = ct$p.value, n = nrow(df))
  }) %>% bind_rows()
}

layer1_cor <- bind_rows(
  lapply(c("wil_of_engage", "seper_recyc"), function(ws) {
    lapply(spill_vars, function(sp) get_cor_kendall(ws_full, sp, ws)) %>% bind_rows()
  })
) %>% mutate(sig = sig_star(p_value))

## R² 对比
r2_comparison <- lapply(spill_years, function(y) {
  extract_r2 <- function(res_list, model_tag) {
    sm  <- summary(res_list[[y]]$fit)
    r2  <- as.data.frame(sm$paths)["R^2", , drop = FALSE]
    data.frame(year = y, model = model_tag,
               Construct = colnames(r2), R2 = as.numeric(r2)) %>%
      filter(!is.na(R2), R2 > 0)
  }
  bind_rows(extract_r2(base_results,   "M_base"),
            extract_r2(bi_results,     "M_spill_bi"),
            extract_r2(direct_results, "M_spill_direct"),
            extract_r2(full_results,   "M_spill_full"))
}) %>% bind_rows()

## 汇总溢出路径
all_spill_paths <- bind_rows(base_paths, bi_paths, direct_paths, full_paths)
key_paths <- all_spill_paths %>%
  filter(grepl("GreenBehav", path)) %>%
  select(model, year, path, beta, se, ci_low, ci_high, p_value, sig) %>%
  arrange(path, model, year)

## 保存CSV
write.csv(layer1_cor,        file.path(out_dir, "spillover_correlation_table.csv"),  row.names = FALSE)
write.csv(all_spill_paths,   file.path(out_dir, "spillover_all_paths.csv"),          row.names = FALSE)
write.csv(key_paths,         file.path(out_dir, "spillover_key_paths.csv"),          row.names = FALSE)
write.csv(mediation_results, file.path(out_dir, "spillover_mediation.csv"),          row.names = FALSE)
write.csv(r2_comparison,     file.path(out_dir, "spillover_r2_comparison.csv"),      row.names = FALSE)
cat("Saved: spillover CSVs\n")

# ============================================================================
# 14. 溢出图形
# ============================================================================

## Layer 1 相关图
p_layer1 <- layer1_cor %>%
  filter(!is.na(tau)) %>%
  mutate(
    var_from  = recode(var_from, reuse_bag = "Reuse bag",
                       energy_concern = "Energy concern",
                       save_energy = "Save water/energy"),
    var_to    = recode(var_to, wil_of_engage = "Intention",
                       seper_recyc = "Behavior"),
    sig_label = paste0(round(tau, 2), sig)
  ) %>%
  ggplot(aes(x = year, y = tau, fill = var_from)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = sig_label), position = position_dodge(0.7),
            vjust = -0.3, size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~ var_to) +
  scale_fill_manual(values = c("#4DBBD5", "#00A087", "#E64B35")) +
  labs(title = "Layer 1: Correlations between green behaviours and waste sorting",
       x = "Year", y = "Kendall's tau", fill = "Green behaviour") +
  theme_classic(base_size = 9) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "gray90"))

ggsave(file.path(out_dir, "spillover_layer1_correlations.pdf"), p_layer1, width = 8, height = 4)

## Layer 2/3 溢出路径图：3列（模型）× N行（路径），空面板留白
spill_model_colors <- c(
  "M_spill_bi"     = "#4DBBD5",
  "M_spill_direct" = "#E64B35",
  "M_spill_full"   = "#3C5488"
)
spill_model_labels <- c(
  "M_spill_bi"     = "Via Intention",
  "M_spill_direct" = "Direct Only",
  "M_spill_full"   = "Full (Both)"
)

spill_path_labels <- c(
  "DESC_NORM->ATT"             = "Desc. Norm -> Attitude",
  "DESC_NORM->PBC"             = "Desc. Norm -> PBC",
  "PBC->ATT"                   = "PBC -> Attitude",
  "DESC_NORM->wil_of_engage"   = "Desc. Norm -> Intention",
  "PBC->wil_of_engage"         = "PBC -> Intention",
  "ATT->wil_of_engage"         = "Attitude -> Intention",
  "wil_of_engage->seper_recyc" = "Intention -> Behavior",
  "PBC->seper_recyc"           = "PBC -> Behavior (direct)",
  "GreenBehav->wil_of_engage"  = "GreenBehav -> Intention",
  "GreenBehav->seper_recyc"    = "GreenBehav -> Behavior"
)

# 路径行顺序
spill_path_order <- c(
  "Desc. Norm -> Attitude", "Desc. Norm -> PBC", "PBC -> Attitude",
  "Desc. Norm -> Intention", "PBC -> Intention", "Attitude -> Intention",
  "Intention -> Behavior", "PBC -> Behavior (direct)",
  "GreenBehav -> Intention", "GreenBehav -> Behavior"
)

all_spill_colors <- c(
  path_colors,
  "GreenBehav -> Intention" = "#4DBBD5",
  "GreenBehav -> Behavior"  = "#E64B35"
)
all_spill_fill <- c(all_spill_colors, ns = "white")

# 整理所有模型路径数据（含M_base）
spill_model_all_labels <- c("M_base" = "Base Model", spill_model_labels)

spill_plot_data <- all_spill_paths %>%
  filter(model %in% names(spill_model_all_labels)) %>%
  mutate(
    path_key   = trimws(gsub("  ->  ", "->", path)),
    Path_full  = factor(spill_path_labels[path_key], levels = spill_path_order),
    model_lab  = factor(spill_model_all_labels[model], levels = unname(spill_model_all_labels)),
    year_num   = as.numeric(year),
    sig_shape  = p_value < 0.05,
    point_fill = if_else(sig_shape, as.character(Path_full), "ns")
  ) %>%
  filter(!is.na(Path_full))

# 全局统一Y轴范围（所有路径、所有模型）
global_ylo <- min(spill_plot_data$ci_low,  na.rm = TRUE)
global_yhi <- max(spill_plot_data$ci_high, na.rm = TRUE)
global_pad <- (global_yhi - global_ylo) * 0.10
global_ylim <- c(global_ylo - global_pad, global_yhi + global_pad)
global_brk  <- pretty(global_ylim, n = 6)
global_brk  <- global_brk[global_brk >= global_ylim[1] & global_brk <= global_ylim[2]]
global_span <- diff(global_ylim)

n_rows   <- length(spill_path_order)
n_cols   <- length(spill_model_all_labels)
panel_h_spill <- rep(global_span * HEIGHT_UNIT, n_rows)

y_scales_spill <- rep(
  list(scale_y_continuous(limits = global_ylim, breaks = global_brk)),
  n_rows * n_cols
)

p_spill_paths <- ggplot(spill_plot_data,
                         aes(x = year_num, y = beta, color = Path_full)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = Path_full),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(aes(fill = point_fill), shape = 21, size = 3.5, stroke = 1) +
  geom_text(aes(label = sig), vjust = -1.0, size = 3, show.legend = FALSE) +
  facet_grid(Path_full ~ model_lab) +
  ggh4x::facetted_pos_scales(y = y_scales_spill) +
  ggh4x::force_panelsizes(rows = unit(panel_h_spill, "in")) +
  scale_x_continuous(breaks = 2021:2023) +
  scale_color_manual(values = all_spill_colors, guide = "none") +
  scale_fill_manual(values = all_spill_fill, guide = "none") +
  labs(
    title    = "Spillover models: path coefficients by model (2021–2023)",
    subtitle = paste0("Bootstrap n = ", N_BOOT,
                      "  |  filled = p < .05, white = n.s.  |  Unified Y-axis scale across all panels"),
    x = "Year", y = "Standardized Coefficient"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x      = element_text(angle = 90, size = 11),
    strip.text.x     = element_text(size = 12),
    strip.text.y     = element_text(size = 11, angle = 0, hjust = 0),
    strip.background = element_rect(fill = "gray85", color = "gray50"),
    panel.border     = element_rect(color = "gray50", fill = NA, linewidth = 0.5)
  )

spill_total_h <- sum(panel_h_spill, na.rm = TRUE)
ggsave(file.path(out_dir, "spillover_layer23_paths.pdf"), p_spill_paths,
       width = 3.5 * n_cols + 1, height = spill_total_h)
cat("Saved: spillover_layer23_paths.pdf\n")

## R² 提升图
p_r2_lift <- r2_comparison %>%
  filter(Construct == "seper_recyc") %>%
  mutate(model = factor(model,
    levels = c("M_base", "M_spill_bi", "M_spill_direct", "M_spill_full"),
    labels = c("M_base\n(TPB only)", "M_spill_bi\n(+GB->BI)",
               "M_spill_direct\n(+GB->BEH)", "M_spill_full\n(+both)"))) %>%
  ggplot(aes(x = year, y = R2, fill = model)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(R2, 3)), position = position_dodge(0.7),
            vjust = -0.3, size = 2.5) +
  scale_fill_manual(values = c("gray60", "#4DBBD5", "#E64B35", "#3C5488")) +
  labs(title = "R2 for seper_recyc across spillover models",
       x = "Year", y = "R2", fill = "Model") +
  theme_classic(base_size = 9) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "spillover_r2_lift.pdf"), p_r2_lift, width = 7, height = 4)


cat("Saved: spillover plots\n")

# ============================================================================
# 完成
# ============================================================================
cat("\n========================================\n")
cat("All outputs saved to:", out_dir, "\n")
cat("========================================\n")
cat("  demographics_table.csv\n")
cat("  group_differences_table.csv\n")
cat("  mean_intension_behavior_of_group.csv\n")
cat("  mean_intension_behavior_of_group_statistic.csv\n")
cat("  pls_reliability_results.csv\n")
cat("  pls_validity_results.csv\n")
cat("  pls_htmt_results.csv\n")
cat("  pls_sem_path_coefficients.csv\n")
cat("  pls_r2_results.csv\n")
cat("  pls_mga_all_pairs.csv\n")
cat("  pls_model_path_plot.pdf\n")
cat("  pls_mga_heatmap.pdf / .png\n")
cat("  spillover_correlation_table.csv\n")
cat("  spillover_all_paths.csv\n")
cat("  spillover_key_paths.csv\n")
cat("  spillover_mediation.csv\n")
cat("  spillover_r2_comparison.csv\n")
cat("  spillover_layer1_correlations.pdf\n")
cat("  spillover_layer23_paths.pdf\n")
cat("  spillover_r2_lift.pdf\n")
