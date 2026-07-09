# ============================================================================
# Alternative Model: 完整 TPB（仅 DESC_NORM）+ 前因两两有向联系
#
# 模型结构：
#   完整 TPB 基础（DESC_NORM 替代 SN）：
#     DESC_NORM → BI
#     PBC       → BI
#     ATT       → BI
#     BI        → BEH
#     PBC       → BEH  （直接路径，对应原图虚线）
#   新增前因两两有向联系：
#     DESC_NORM → ATT
#     DESC_NORM → PBC
#     PBC       → ATT
#
# 输出目录：data_proc/result_<YYYYMMDD_HHMMSS>/
# ============================================================================

pacman::p_load(
  dplyr, stringr, tidyr, patchwork, ggplot2, readxl, purrr, showtext,
  psych, seminr
)
showtext::showtext_auto()

out_dir <- file.path("data_proc", format(Sys.time(), "result_%Y%m%d_%H%M%S"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
cat("Output directory:", out_dir, "\n")

# ----------------------------------------------------------------------------
# 1. 数据加载（与 combined_analysis_pls.R 完全相同的管道）
# ----------------------------------------------------------------------------
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

# ----------------------------------------------------------------------------
# 2. 构念与变量定义
# ----------------------------------------------------------------------------
att_vars       <- c("ws_attitude", "ws_interest", "threat")
desc_norm_vars <- c("if_neighbor_ws", "if_family_ws")
pbc_vars       <- c("category_trouble", "time_cost_troub")
model_vars     <- c(att_vars, desc_norm_vars, pbc_vars)

# ----------------------------------------------------------------------------
# 3. PLS 测量模型与结构模型
# ----------------------------------------------------------------------------
pls_mm <- constructs(
  reflective("ATT",           att_vars),
  reflective("DESC_NORM",     desc_norm_vars),
  reflective("PBC",           pbc_vars),
  reflective("wil_of_engage", single_item("wil_of_engage")),
  reflective("seper_recyc",   single_item("seper_recyc"))
)

# 模型结构：
#   前因两两：DESC_NORM → PBC, DESC_NORM → ATT, PBC → ATT
#   核心链：  ATT → BI → BEH
#   直接到行为：DESC_NORM → BEH, PBC → BEH, ATT → BEH
pls_sm <- relationships(
  # 完整 TPB 核心路径（DESC_NORM 替代 SN）
  paths(from = c("DESC_NORM", "PBC", "ATT"), to = "wil_of_engage"),
  paths(from = "wil_of_engage", to = "seper_recyc"),
  paths(from = "PBC",           to = "seper_recyc"),  # PBC 直接→BEH（虚线）
  # 新增：前因构念两两有向联系
  paths(from = "DESC_NORM", to = "ATT"),
  paths(from = "DESC_NORM", to = "PBC"),
  paths(from = "PBC",       to = "ATT")
)

# ----------------------------------------------------------------------------
# 4. 可靠性辅助函数
# ----------------------------------------------------------------------------
get_alpha <- function(df, vars, label) {
  res <- psych::alpha(df[, vars], check.keys = TRUE)
  data.frame(Construct = label, Alpha = res$total$raw_alpha)
}

sig_star <- function(p) case_when(
  p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ ""
)

# ----------------------------------------------------------------------------
# 5. 逐年拟合 PLS + Bootstrap
# ----------------------------------------------------------------------------
N_BOOT      <- 1000
year_levels <- levels(ws_full$year)

fit_pls_year <- function(df_year, year_label) {
  cat(paste0("  Fitting model for year: ", year_label, "\n"))
  df_pls <- df_year %>%
    select(all_of(c(model_vars, "wil_of_engage", "seper_recyc"))) %>%
    mutate(across(everything(), as.numeric)) %>%
    na.omit() %>%
    as.data.frame()
  cat(paste0("    Complete cases: ", nrow(df_pls), "\n"))

  pls_fit  <- estimate_pls(
    data = df_pls, measurement_model = pls_mm,
    structural_model = pls_sm, inner_weights = path_weighting
  )
  boot_fit <- bootstrap_model(pls_fit, nboot = N_BOOT, seed = 42)
  list(pls_fit = pls_fit, boot_fit = boot_fit, df_pls = df_pls, year = year_label)
}

# Cronbach's alpha 逐年
reliability_results <- lapply(year_levels, function(y) {
  df_year <- ws_full %>% filter(year == y)
  bind_rows(
    get_alpha(df_year, att_vars,       "ATT"),
    get_alpha(df_year, desc_norm_vars, "DESC_NORM"),
    get_alpha(df_year, pbc_vars,       "PBC")
  ) %>% mutate(year = y)
}) %>% bind_rows()

print(reliability_results)

# 逐年拟合
pls_results <- lapply(year_levels, function(y) {
  fit_pls_year(ws_full %>% filter(year == y), y)
})
names(pls_results) <- year_levels

# ----------------------------------------------------------------------------
# 6. 提取 CR / AVE / HTMT
# ----------------------------------------------------------------------------
validity_results <- lapply(year_levels, function(y) {
  sm  <- summary(pls_results[[y]]$pls_fit)
  rel <- as.data.frame(sm$reliability)
  rel$Construct <- rownames(rel)
  rel %>%
    mutate(year = y) %>%
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

write.csv(reliability_results, file.path(out_dir, "pls_reliability_results.csv"), row.names = FALSE)
write.csv(validity_results,    file.path(out_dir, "pls_validity_results.csv"),    row.names = FALSE)
write.csv(htmt_results,        file.path(out_dir, "pls_htmt_results.csv"),        row.names = FALSE)

# ----------------------------------------------------------------------------
# 7. 路径系数（Bootstrap）
# ----------------------------------------------------------------------------
path_results <- lapply(year_levels, function(y) {
  sm <- summary(pls_results[[y]]$boot_fit)
  bp <- as.data.frame(sm$bootstrapped_paths)
  bp$path_raw <- rownames(bp)
  bp %>%
    transmute(
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

cat("\nPath Coefficients (bootstrapped):\n")
print(as.data.frame(path_results))
write.csv(path_results, file.path(out_dir, "pls_sem_path_coefficients.csv"), row.names = FALSE)

# ----------------------------------------------------------------------------
# 8. R²
# ----------------------------------------------------------------------------
r2_results <- lapply(year_levels, function(y) {
  sm     <- summary(pls_results[[y]]$pls_fit)
  r2_df  <- as.data.frame(sm$paths)
  r2_vals <- r2_df["R^2", , drop = FALSE]
  data.frame(
    year = y, Construct = colnames(r2_vals), R2 = as.numeric(r2_vals)
  )
}) %>% bind_rows() %>% filter(!is.na(R2), R2 > 0)

print("R²:"); print(r2_results)
write.csv(r2_results, file.path(out_dir, "pls_r2_results.csv"), row.names = FALSE)

# ----------------------------------------------------------------------------
# 9. MGA：所有年份两两比较
# ----------------------------------------------------------------------------
cat("\nRunning MGA for all year pairs...\n")
all_pairs <- combn(year_levels, 2, simplify = FALSE)

mga_results <- lapply(all_pairs, function(pair) {
  ya <- pair[1]; yb <- pair[2]
  cat(paste0("  MGA: ", ya, " vs ", yb, "\n"))

  df_a <- pls_results[[ya]]$df_pls
  df_b <- pls_results[[yb]]$df_pls
  df_combined <- bind_rows(df_a, df_b)

  pls_combined <- estimate_pls(
    data = df_combined, measurement_model = pls_mm,
    structural_model = pls_sm, inner_weights = path_weighting
  )
  condition <- c(rep(TRUE, nrow(df_a)), rep(FALSE, nrow(df_b)))

  tryCatch({
    mga <- estimate_pls_mga(pls_combined, condition, nboot = N_BOOT)
    mga$year_a <- ya; mga$year_b <- yb
    mga$sig    <- sig_star(mga$pls_mga_p)
    as.data.frame(mga)
  }, error = function(e) {
    cat("    Error:", conditionMessage(e), "\n"); NULL
  })
}) %>% bind_rows()

print(as.data.frame(mga_results))
write.csv(mga_results, file.path(out_dir, "pls_mga_all_pairs.csv"), row.names = FALSE)

# ----------------------------------------------------------------------------
# 10. 图：路径系数年份趋势
# ----------------------------------------------------------------------------
path_labels <- c(
  # 完整 TPB 核心路径
  "DESC_NORM->wil_of_engage"    = "Desc. Norm -> Intention",
  "PBC->wil_of_engage"          = "PBC -> Intention",
  "ATT->wil_of_engage"          = "Attitude -> Intention",
  "wil_of_engage->seper_recyc"  = "Intention -> Behavior",
  "PBC->seper_recyc"            = "PBC -> Behavior (direct)",
  # 新增两两联系
  "DESC_NORM->ATT"              = "Desc. Norm -> Attitude",
  "DESC_NORM->PBC"              = "Desc. Norm -> PBC",
  "PBC->ATT"                    = "PBC -> Attitude"
)

path_colors <- c(
  "Desc. Norm -> Intention"    = "#3C5488",
  "PBC -> Intention"           = "#00A087",
  "Attitude -> Intention"      = "#4DBB15",
  "Intention -> Behavior"      = "#E64B35",
  "PBC -> Behavior (direct)"   = "#E67E22",
  "Desc. Norm -> Attitude"     = "#8E44AD",
  "Desc. Norm -> PBC"          = "#9B59B6",
  "PBC -> Attitude"            = "#1ABC9C"
)

plot_data <- path_results %>%
  mutate(
    path_key  = paste0(rhs, "->", lhs),
    Path_full = path_labels[path_key],
    year_num  = as.numeric(year),
    sig_shape = p_value < 0.05
  ) %>%
  filter(!is.na(Path_full)) %>%
  mutate(Path_full = factor(Path_full, levels = names(path_colors)))

p_paths <- ggplot(plot_data, aes(x = year_num, y = beta, color = Path_full)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = Path_full),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(aes(shape = sig_shape), size = 3.5, stroke = 1) +
  geom_text(aes(label = sig), vjust = -1.0, size = 3, show.legend = FALSE) +
  facet_wrap(~ Path_full, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = 2019:2023) +
  scale_shape_manual(
    values = c("TRUE" = 16, "FALSE" = 1),
    labels = c("TRUE" = "p < .05", "FALSE" = "n.s."),
    name   = NULL
  ) +
  scale_color_manual(values = path_colors, guide = "none") +
  scale_fill_manual(values  = path_colors, guide = "none") +
  labs(
    title = "Alt Model: Full TPB (DESC_NORM only) + pairwise connections among antecedents",
    x = "Year",
    y = paste0("Standardized Coefficient (bootstrap n = ", N_BOOT, ", 95% CI)")
  ) +
  theme_classic(base_size = 9) +
  theme(
    axis.text.x      = element_text(angle = 90),
    strip.background = element_rect(fill = "gray85", color = "gray50"),
    legend.position  = "bottom",
    panel.border     = element_rect(color = "gray50", fill = NA, linewidth = 0.5)
  )

ggsave(file.path(out_dir, "pls_model_path_plot.pdf"), plot = p_paths, width = 6, height = 14)
cat("Path plot saved.\n")

# ----------------------------------------------------------------------------
# 11. 图：MGA 热力图
# ----------------------------------------------------------------------------
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
    sig_level  = factor(sig_level, levels = c("p < .001", "p < .01", "p < .05", "n.s."))
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
    title    = "PLS-MGA: Path Coefficient Differences Between Years (Alt Model)",
    subtitle = paste0("All C(5,2)=10 year pairs  |  bootstrap n = ", N_BOOT,
                      "  |  red = significant difference (p < .05)"),
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 10)

ggsave(file.path(out_dir, "pls_mga_heatmap.pdf"), p_heatmap, width = 14, height = 14)
ggsave(file.path(out_dir, "pls_mga_heatmap.png"), p_heatmap, width = 14, height = 14, dpi = 180)
cat("MGA heatmap saved.\n")

# ----------------------------------------------------------------------------
# 完成
# ----------------------------------------------------------------------------
cat("\n=== Alt model analysis complete ===\n")
cat("Output files saved to:", out_dir, "\n")
cat("  pls_reliability_results.csv\n")
cat("  pls_validity_results.csv\n")
cat("  pls_htmt_results.csv\n")
cat("  pls_sem_path_coefficients.csv\n")
cat("  pls_r2_results.csv\n")
cat("  pls_mga_all_pairs.csv\n")
cat("  pls_model_path_plot.pdf\n")
cat("  pls_mga_heatmap.pdf / .png\n")
