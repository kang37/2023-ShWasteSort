# ============================================================================
# Spillover Effect Analysis Script
# Research question: Do other pro-environmental behaviours (reuse_bag,
# energy_concern, save_energy) predict waste-sorting beyond the TPB mechanism?
#
# Two-layer approach (on 2021-2023 data where spillover vars are available):
#   Layer 2 – Direct spillover: GreenBehav → seper_recyc (controlling TPB path)
#   Layer 3 – Mechanism:       GreenBehav → ATT (attitudinal/cognitive route)
#                               vs. GreenBehav → seper_recyc (habitual route)
#             Bootstrapped mediation: indirect effect via ATT and wil_of_engage
#
# Baseline model (M7, replicated here):
#   INJ_NORM, DESC_NORM, PBC → ATT → wil_of_engage → seper_recyc
#
# Spillover construct (GreenBehav): composite of reuse_bag, energy_concern,
# save_energy. Three distinct behaviours → composite (formative) specification.
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Packages ----
# ----------------------------------------------------------------------------
pacman::p_load(
  dplyr, stringr, tidyr, patchwork, ggplot2, readxl, purrr,
  psych, seminr, showtext
)
showtext::showtext_auto()

# ----------------------------------------------------------------------------
# 2. Data (identical pipeline as combined_analysis_pls.R) ----
# ----------------------------------------------------------------------------
colname_mapping <- read_excel("data_raw/colname_mapping.xlsx")

create_rename_vector <- function(mapping_df, year) {
  col_year <- paste0("col_", year)
  mapping_df %>%
    filter(!is.na(.data[[col_year]])) %>%
    select(old_name = all_of(col_year), new_name = unified_name_en) %>%
    { setNames(.$old_name, .$new_name) }
}

years   <- 2019:2023
ws_list <- map2(
  paste0("data_raw/SHWS", years, ".xlsx"), years,
  function(file, year) {
    read_excel(file) %>%
      rename(any_of(create_rename_vector(colname_mapping, year))) %>%
      mutate(year = as.factor(year))
  }
)

vars_2019_recode <- c(
  "info_qual_tv", "info_qual_news", "info_qual_web", "info_qual_ad",
  "info_qual_gov", "info_qual_school", "info_qual_street",
  "info_qual_volunteer", "info_qual_neighbor", "info_qual_family",
  "info_qual_property",
  "freq_online_secondhand", "freq_gov", "freq_ngo", "freq_media",
  "freq_school", "freq_waste_co", "freq_resident", "freq_street",
  "freq_property", "freq_supervisor",
  "role_gov", "role_ngo", "role_media", "role_school", "role_waste_co",
  "role_resident", "role_street", "role_property", "role_supervisor",
  "support_gov", "support_ngo", "support_media", "support_school",
  "support_waste_co", "support_resident", "support_street",
  "support_property", "support_supervisor"
)

recode_2019_letters <- function(x) {
  case_when(
    str_detect(x, "^a") ~ 1, str_detect(x, "^b") ~ 2,
    str_detect(x, "^c") ~ 3, str_detect(x, "^d") ~ 4,
    str_detect(x, "^e") ~ 5,
    x == "-3" | x == -3  ~ NA_real_,
    TRUE ~ as.numeric(x)
  )
}
ws_list[[1]] <- ws_list[[1]] %>%
  mutate(across(any_of(vars_2019_recode), recode_2019_letters))
ws_list[[2]] <- ws_list[[2]] %>% mutate(across(any_of("elder_num"), as.numeric))
ws_list[[4]] <- ws_list[[4]] %>% mutate(across(any_of(c("elder_num", "age")), as.numeric))
ws_list[[5]] <- ws_list[[5]] %>% mutate(across(any_of("elder_num"), as.numeric))

ws_full <- bind_rows(ws_list) %>%
  mutate(
    across(
      .cols = all_of(
        colname_mapping %>% filter(var_scale5_rev == 1) %>% pull(unified_name_en)
      ),
      .fns = ~ { x <- as.numeric(.x); 5 + 1 - x }
    ),
    gender  = as.factor(na_if(gender, -2)),
    age_grp = case_when(
      age <= 25  ~ "<=25", age <= 40 ~ "25~40", age <= 60 ~ "40_60",
      age <= 100 ~ ">60",  age >= 100 ~ NA_character_
    )
  )

# ----------------------------------------------------------------------------
# 3. Variable definitions ----
# ----------------------------------------------------------------------------
# M7 TPB constructs
att_vars       <- c("ws_attitude", "ws_interest", "threat")
inj_norm_vars  <- c("pr_atten", "regulate_law", "regulate_commu_rule")
desc_norm_vars <- c("if_neighbor_ws", "if_family_ws")
pbc_vars       <- c("category_trouble", "time_cost_troub")
tpb_vars       <- c(att_vars, inj_norm_vars, desc_norm_vars, pbc_vars)
outcome_vars   <- c("wil_of_engage", "seper_recyc")

# Spillover construct: other pro-environmental behaviours
# Available 2021–2023 only
spill_vars <- c("reuse_bag", "energy_concern", "save_energy")

# Years with spillover data
spill_years <- c("2021", "2022", "2023")

# Verify variables exist
cat("=== Variable availability check ===\n")
missing_tpb   <- setdiff(c(tpb_vars, outcome_vars), names(ws_full))
missing_spill <- setdiff(spill_vars, names(ws_full))
if (length(missing_tpb))   cat("Missing TPB vars:", paste(missing_tpb,   collapse=", "), "\n")
if (length(missing_spill)) cat("Missing spill vars:", paste(missing_spill, collapse=", "), "\n")
if (!length(c(missing_tpb, missing_spill))) cat("All variables present.\n\n")

# Check actual availability per year
cat("Spillover variable availability by year:\n")
ws_full %>%
  group_by(year) %>%
  summarise(across(all_of(spill_vars), ~ sum(!is.na(.))), .groups = "drop") %>%
  print()
cat("\n")

# ----------------------------------------------------------------------------
# 4. Descriptive correlations (Layer 1 – baseline) ----
# ----------------------------------------------------------------------------
cat("=== Layer 1: Descriptive correlations ===\n")

get_cor_kendall <- function(data, var_x, var_y) {
  lapply(levels(data$year), function(y) {
    df <- data %>%
      filter(year == y, !is.na(.data[[var_x]]), !is.na(.data[[var_y]]))
    if (nrow(df) < 10) {
      return(tibble(year=y, var_from=var_x, var_to=var_y,
                    tau=NA_real_, p_value=NA_real_, n=nrow(df)))
    }
    ct <- cor.test(df[[var_x]], df[[var_y]], method = "kendall")
    tibble(year=y, var_from=var_x, var_to=var_y,
           tau=ct$estimate, p_value=ct$p.value, n=nrow(df))
  }) %>% bind_rows()
}

layer1_cor <- bind_rows(
  lapply(c("wil_of_engage", "seper_recyc"), function(ws) {
    lapply(spill_vars, function(sp) get_cor_kendall(ws_full, sp, ws)) %>% bind_rows()
  })
) %>%
  mutate(sig = case_when(
    p_value < .001 ~ "***", p_value < .01 ~ "**",
    p_value < .05  ~ "*",   TRUE ~ ""
  ))

print(layer1_cor)
write.csv(layer1_cor, "data_proc/spillover_layer1_correlations.csv", row.names=FALSE)

# ----------------------------------------------------------------------------
# 5. PLS model helpers ----
# ----------------------------------------------------------------------------
N_BOOT <- 1000  # bootstrap replications for significance and mediation

# Build a data frame for PLS (complete cases only)
make_pls_df <- function(data, year_val, extra_vars = character(0)) {
  all_v <- c(tpb_vars, outcome_vars, extra_vars)
  data %>%
    filter(year == year_val) %>%
    select(all_of(intersect(all_v, names(data)))) %>%
    mutate(across(everything(), as.numeric)) %>%
    na.omit() %>%
    as.data.frame()
}

# Extract path rows from seminr bootstrap summary
extract_paths_boot <- function(boot_sum, year_val, model_tag) {
  # boot_sum$bootstrapped_paths: matrix with rows = paths, cols include Mean, SD, CI
  bp <- as.data.frame(boot_sum$bootstrapped_paths)
  bp$path_label <- rownames(bp)
  bp %>%
    transmute(
      year      = year_val,
      model     = model_tag,
      path      = path_label,
      beta      = `Bootstrap Mean`,
      se        = `Bootstrap SD`,
      t_stat    = `Bootstrap Mean` / `Bootstrap SD`,
      p_value   = 2 * pt(-abs(`Bootstrap Mean` / `Bootstrap SD`), df = N_BOOT - 1),
      ci_low    = `2.5% CI`,
      ci_high   = `97.5% CI`
    )
}

sig_star <- function(p) case_when(
  p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ ""
)

# ----------------------------------------------------------------------------
# 6. M_base: replicate M7 on 2021-2023 for comparability ----
# ----------------------------------------------------------------------------
cat("=== Fitting M_base (M7 replication, 2021-2023) ===\n")

pls_mm_base <- constructs(
  reflective("ATT",          att_vars),
  reflective("INJ_NORM",     inj_norm_vars),
  reflective("DESC_NORM",    desc_norm_vars),
  reflective("PBC",          pbc_vars),
  reflective("wil_of_engage", single_item("wil_of_engage")),
  reflective("seper_recyc",   single_item("seper_recyc"))
)
pls_sm_base <- relationships(
  paths(from = c("INJ_NORM", "DESC_NORM", "PBC"), to = "ATT"),
  paths(from = "ATT",           to = "wil_of_engage"),
  paths(from = "wil_of_engage", to = "seper_recyc")
)

base_results <- lapply(spill_years, function(y) {
  cat("  Year:", y, "\n")
  df <- make_pls_df(ws_full, y)
  cat("    n =", nrow(df), "\n")
  fit <- estimate_pls(df, pls_mm_base, pls_sm_base, inner_weights = path_weighting)
  boot <- bootstrap_model(fit, nboot = N_BOOT, seed = 42)
  list(fit = fit, boot = boot, year = y)
})
names(base_results) <- spill_years

base_paths <- lapply(spill_years, function(y) {
  extract_paths_boot(summary(base_results[[y]]$boot), y, "M_base")
}) %>% bind_rows() %>% mutate(sig = sig_star(p_value))

cat("\nM_base path coefficients:\n")
print(base_paths %>% select(year, path, beta, se, p_value, sig))

# ----------------------------------------------------------------------------
# 7. Layer 2 – Direct spillover: GreenBehav → seper_recyc ----
# ----------------------------------------------------------------------------
# Specification: GreenBehav as composite (formative) because reuse_bag,
# energy_concern, save_energy are distinct behaviours, not reflective indicators.
# Testing: does prior green behaviour directly predict recycling beyond TPB?
cat("\n=== Layer 2: M_spill_direct – GreenBehav → seper_recyc ===\n")

pls_mm_direct <- constructs(
  reflective("ATT",          att_vars),
  reflective("INJ_NORM",     inj_norm_vars),
  reflective("DESC_NORM",    desc_norm_vars),
  reflective("PBC",          pbc_vars),
  composite("GreenBehav",   spill_vars, weights = correlation_weights),
  reflective("wil_of_engage", single_item("wil_of_engage")),
  reflective("seper_recyc",   single_item("seper_recyc"))
)
pls_sm_direct <- relationships(
  paths(from = c("INJ_NORM", "DESC_NORM", "PBC"), to = "ATT"),
  paths(from = "ATT",           to = "wil_of_engage"),
  paths(from = "wil_of_engage", to = "seper_recyc"),
  paths(from = "GreenBehav",    to = "seper_recyc")   # direct spillover path
)

direct_results <- lapply(spill_years, function(y) {
  cat("  Year:", y, "\n")
  df <- make_pls_df(ws_full, y, extra_vars = spill_vars)
  cat("    n =", nrow(df), "\n")
  fit  <- estimate_pls(df, pls_mm_direct, pls_sm_direct, inner_weights = path_weighting)
  boot <- bootstrap_model(fit, nboot = N_BOOT, seed = 42)
  list(fit = fit, boot = boot, year = y)
})
names(direct_results) <- spill_years

direct_paths <- lapply(spill_years, function(y) {
  extract_paths_boot(summary(direct_results[[y]]$boot), y, "M_spill_direct")
}) %>% bind_rows() %>% mutate(sig = sig_star(p_value))

cat("\nM_spill_direct path coefficients:\n")
print(direct_paths %>% select(year, path, beta, se, p_value, sig))

# ----------------------------------------------------------------------------
# 8. Layer 3 – Attitudinal route: GreenBehav → ATT ----
# ----------------------------------------------------------------------------
# Mechanism question: does green behaviour spill over by changing attitudes
# (self-perception / cognitive consistency mechanism)?
cat("\n=== Layer 3a: M_spill_att – GreenBehav → ATT ===\n")

pls_sm_att <- relationships(
  paths(from = c("INJ_NORM", "DESC_NORM", "PBC"), to = "ATT"),
  paths(from = "GreenBehav",    to = "ATT"),          # attitudinal spillover
  paths(from = "ATT",           to = "wil_of_engage"),
  paths(from = "wil_of_engage", to = "seper_recyc")
)

att_results <- lapply(spill_years, function(y) {
  cat("  Year:", y, "\n")
  df <- make_pls_df(ws_full, y, extra_vars = spill_vars)
  fit  <- estimate_pls(df, pls_mm_direct, pls_sm_att, inner_weights = path_weighting)
  boot <- bootstrap_model(fit, nboot = N_BOOT, seed = 42)
  list(fit = fit, boot = boot, year = y)
})
names(att_results) <- spill_years

att_paths <- lapply(spill_years, function(y) {
  extract_paths_boot(summary(att_results[[y]]$boot), y, "M_spill_att")
}) %>% bind_rows() %>% mutate(sig = sig_star(p_value))

cat("\nM_spill_att path coefficients:\n")
print(att_paths %>% select(year, path, beta, se, p_value, sig))

# ----------------------------------------------------------------------------
# 9. Layer 3 – Full model: both routes ----
# ----------------------------------------------------------------------------
cat("\n=== Layer 3b: M_spill_full – GreenBehav → ATT + GreenBehav → seper_recyc ===\n")

pls_sm_full <- relationships(
  paths(from = c("INJ_NORM", "DESC_NORM", "PBC"), to = "ATT"),
  paths(from = "GreenBehav",    to = "ATT"),
  paths(from = "ATT",           to = "wil_of_engage"),
  paths(from = "wil_of_engage", to = "seper_recyc"),
  paths(from = "GreenBehav",    to = "seper_recyc")
)

full_results <- lapply(spill_years, function(y) {
  cat("  Year:", y, "\n")
  df <- make_pls_df(ws_full, y, extra_vars = spill_vars)
  fit  <- estimate_pls(df, pls_mm_direct, pls_sm_full, inner_weights = path_weighting)
  boot <- bootstrap_model(fit, nboot = N_BOOT, seed = 42)
  list(fit = fit, boot = boot, year = y)
})
names(full_results) <- spill_years

full_paths <- lapply(spill_years, function(y) {
  extract_paths_boot(summary(full_results[[y]]$boot), y, "M_spill_full")
}) %>% bind_rows() %>% mutate(sig = sig_star(p_value))

cat("\nM_spill_full path coefficients:\n")
print(full_paths %>% select(year, path, beta, se, p_value, sig))

# ----------------------------------------------------------------------------
# 10. Bootstrapped mediation: indirect effects via ATT and wil_of_engage ----
# ----------------------------------------------------------------------------
# In M_spill_full, the spillover path from GreenBehav to seper_recyc can go:
#   Indirect A: GreenBehav → ATT → wil_of_engage → seper_recyc
#   Indirect B: GreenBehav → ATT → wil_of_engage (partial, via attitude only)
#   Direct:     GreenBehav → seper_recyc
# seminr reports specific_effect_significance() for indirect paths.
cat("\n=== Mediation: indirect effects of GreenBehav via ATT ===\n")

# specific_effect_significance() returns a matrix [1, 7] with named columns:
# "Original Est.", "Bootstrap Mean", "Bootstrap SD", "T Stat.",
# "2.5% CI", "97.5% CI", "Bootstrap P Val"
parse_ie <- function(ie_mat, year_val, path_label) {
  if (is.null(ie_mat)) return(NULL)
  tibble(
    year    = year_val,
    path    = path_label,
    beta    = ie_mat[1, "Bootstrap Mean"],
    se      = ie_mat[1, "Bootstrap SD"],
    ci_low  = ie_mat[1, "2.5% CI"],
    ci_high = ie_mat[1, "97.5% CI"],
    p_value = ie_mat[1, "Bootstrap P Val"]
  )
}

mediation_results <- lapply(spill_years, function(y) {
  boot <- full_results[[y]]$boot

  # Full indirect: GreenBehav → ATT → wil_of_engage → seper_recyc
  ie_full <- tryCatch(
    specific_effect_significance(
      boot,
      from    = "GreenBehav",
      through = c("ATT", "wil_of_engage"),
      to      = "seper_recyc",
      alpha   = 0.05
    ),
    error = function(e) { cat("  IE full error:", conditionMessage(e), "\n"); NULL }
  )

  # Partial indirect: GreenBehav → ATT → wil_of_engage
  ie_att_bi <- tryCatch(
    specific_effect_significance(
      boot,
      from    = "GreenBehav",
      through = "ATT",
      to      = "wil_of_engage",
      alpha   = 0.05
    ),
    error = function(e) { cat("  IE att→BI error:", conditionMessage(e), "\n"); NULL }
  )

  bind_rows(
    parse_ie(ie_full,   y, "GreenBehav → ATT → BI → BEH (indirect)"),
    parse_ie(ie_att_bi, y, "GreenBehav → ATT → BI (indirect)")
  )
}) %>% bind_rows() %>% mutate(sig = sig_star(p_value))

cat("\nIndirect (mediated) effects:\n")
print(as.data.frame(mediation_results))

# ----------------------------------------------------------------------------
# 11. Composite summary table ----
# ----------------------------------------------------------------------------
all_paths <- bind_rows(base_paths, direct_paths, att_paths, full_paths)
write.csv(all_paths,         "data_proc/spillover_all_paths.csv",       row.names=FALSE)
write.csv(mediation_results, "data_proc/spillover_mediation.csv",       row.names=FALSE)
write.csv(layer1_cor,        "data_proc/spillover_layer1_correlations.csv", row.names=FALSE)

# Key spillover paths for comparison table
spillover_focus <- all_paths %>%
  filter(grepl("GreenBehav", path)) %>%
  select(model, year, path, beta, se, ci_low, ci_high, p_value, sig) %>%
  arrange(path, model, year)

cat("\n=== Key spillover path coefficients (GreenBehav paths across models) ===\n")
print(as.data.frame(spillover_focus))
write.csv(spillover_focus, "data_proc/spillover_key_paths.csv", row.names=FALSE)

# R² comparison across models
r2_comparison <- lapply(spill_years, function(y) {
  extract_r2 <- function(res_list, model_tag) {
    sm <- summary(res_list[[y]]$fit)
    r2 <- as.data.frame(sm$paths)["R^2", , drop=FALSE]
    data.frame(
      year      = y,
      model     = model_tag,
      Construct = colnames(r2),
      R2        = as.numeric(r2)
    ) %>% filter(!is.na(R2), R2 > 0)
  }
  bind_rows(
    extract_r2(base_results,   "M_base"),
    extract_r2(direct_results, "M_spill_direct"),
    extract_r2(att_results,    "M_spill_att"),
    extract_r2(full_results,   "M_spill_full")
  )
}) %>% bind_rows()

cat("\n=== R² comparison across models ===\n")
print(as.data.frame(r2_comparison %>% tidyr::pivot_wider(names_from=model, values_from=R2)))
write.csv(r2_comparison, "data_proc/spillover_r2_comparison.csv", row.names=FALSE)

# ----------------------------------------------------------------------------
# 12. Plots ----
# ----------------------------------------------------------------------------
cat("\nGenerating plots...\n")

# Plot 1: Layer 1 correlations (Kendall's tau by year)
p_layer1 <- layer1_cor %>%
  filter(!is.na(tau)) %>%
  mutate(
    var_from = recode(var_from,
      reuse_bag      = "Reuse bag",
      energy_concern = "Energy concern",
      save_energy    = "Save water/energy"
    ),
    var_to = recode(var_to,
      wil_of_engage = "Intention",
      seper_recyc   = "Behavior"
    ),
    sig_label = paste0(round(tau, 2), sig)
  ) %>%
  ggplot(aes(x=year, y=tau, fill=var_from)) +
  geom_col(position="dodge", width=0.7) +
  geom_text(aes(label=sig_label), position=position_dodge(0.7), vjust=-0.3, size=2.5) +
  geom_hline(yintercept=0, linetype="dashed", color="gray40") +
  facet_wrap(~var_to) +
  scale_fill_manual(values = c("#4DBBD5","#00A087","#E64B35")) +
  labs(
    title = "Layer 1: Correlations between green behaviours and waste sorting",
    x = "Year", y = "Kendall's tau", fill = "Green behaviour"
  ) +
  theme_classic(base_size=9) +
  theme(legend.position="bottom", strip.background=element_rect(fill="gray90"))

ggsave("data_proc/spillover_layer1_correlations.pdf", p_layer1, width=8, height=4)

# Plot 2: GreenBehav path coefficients across models and years (key spillover paths)
path_labels_map <- c(
  "GreenBehav  ->  seper_recyc" = "GreenBehav → Behavior (direct)",
  "GreenBehav  ->  ATT"         = "GreenBehav → Attitude"
)

p_spill_paths <- spillover_focus %>%
  mutate(
    path_clean = case_when(
      grepl("seper_recyc", path) ~ "GreenBehav → Behavior (direct)",
      grepl("ATT",         path) ~ "GreenBehav → Attitude",
      TRUE ~ path
    ),
    model = factor(model, levels=c("M_spill_direct","M_spill_att","M_spill_full"),
                   labels=c("Direct only","Attitudinal only","Full (both)")),
    year_num = as.numeric(year)
  ) %>%
  ggplot(aes(x=year_num, y=beta, color=model, group=model)) +
  geom_hline(yintercept=0, linetype="dashed", color="gray40", linewidth=0.4) +
  geom_ribbon(aes(ymin=ci_low, ymax=ci_high, fill=model), alpha=0.12, color=NA) +
  geom_line(linewidth=0.9) +
  geom_point(size=3, stroke=1) +
  facet_wrap(~path_clean, ncol=1, scales="free_y") +
  scale_x_continuous(breaks=2021:2023) +
  scale_color_manual(values=c("#E64B35","#4DBBD5","#3C5488")) +
  scale_fill_manual(values=c("#E64B35","#4DBBD5","#3C5488")) +
  labs(
    title = "Layer 2/3: Spillover path coefficients (bootstrapped, 95% CI)",
    x = "Year", y = "Standardised β", color = "Model", fill = "Model"
  ) +
  theme_classic(base_size=9) +
  theme(
    legend.position="bottom", strip.background=element_rect(fill="gray90"),
    panel.border=element_rect(color="gray50", fill=NA, linewidth=0.5)
  )

ggsave("data_proc/spillover_layer23_paths.pdf", p_spill_paths, width=7, height=6)

# Plot 3: R² lift from adding spillover
p_r2 <- r2_comparison %>%
  filter(Construct == "seper_recyc") %>%
  mutate(
    model = factor(model,
      levels=c("M_base","M_spill_att","M_spill_direct","M_spill_full"),
      labels=c("M_base\n(TPB only)","M_spill_att\n(+GB→ATT)",
               "M_spill_direct\n(+GB→BEH)","M_spill_full\n(+both)"))
  ) %>%
  ggplot(aes(x=year, y=R2, fill=model)) +
  geom_col(position="dodge", width=0.7) +
  geom_text(aes(label=round(R2,3)), position=position_dodge(0.7), vjust=-0.3, size=2.5) +
  scale_fill_manual(values=c("gray60","#4DBBD5","#E64B35","#3C5488")) +
  labs(
    title = "R² for seper_recyc (behavior) across spillover models",
    x = "Year", y = "R²", fill = "Model"
  ) +
  theme_classic(base_size=9) +
  theme(legend.position="bottom")

ggsave("data_proc/spillover_r2_lift.pdf", p_r2, width=7, height=4)

# Plot 4: Mediation summary
if (nrow(mediation_results) > 0) {
  p_mediation <- mediation_results %>%
    mutate(
      path = str_wrap(path, 30),
      year_num = as.numeric(year)
    ) %>%
    ggplot(aes(x=year_num, y=beta, color=path, group=path)) +
    geom_hline(yintercept=0, linetype="dashed", color="gray40") +
    geom_ribbon(aes(ymin=ci_low, ymax=ci_high, fill=path), alpha=0.15, color=NA) +
    geom_line(linewidth=0.9) +
    geom_point(size=3) +
    scale_x_continuous(breaks=2021:2023) +
    scale_color_manual(values=c("#4DBBD5","#E64B35")) +
    scale_fill_manual(values=c("#4DBBD5","#E64B35")) +
    labs(
      title = "Mediation: indirect effects of GreenBehav via ATT",
      x = "Year", y = "Indirect effect (bootstrapped)", color=NULL, fill=NULL
    ) +
    theme_classic(base_size=9) +
    theme(legend.position="bottom")

  ggsave("data_proc/spillover_mediation_plot.pdf", p_mediation, width=7, height=4)
}

cat("\n=== Spillover analysis complete ===\n")
cat("Output files saved to data_proc/:\n")
cat("  spillover_layer1_correlations.csv / .pdf\n")
cat("  spillover_all_paths.csv\n")
cat("  spillover_key_paths.csv\n")
cat("  spillover_mediation.csv\n")
cat("  spillover_r2_comparison.csv\n")
cat("  spillover_layer23_paths.pdf\n")
cat("  spillover_r2_lift.pdf\n")
cat("  spillover_mediation_plot.pdf\n")
