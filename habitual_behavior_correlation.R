# Habitual pro-environmental behaviors and waste-sorting behavior
# Kendall correlations by year (2021-2023) and annotated heatmap.

library(dplyr)
library(ggplot2)
library(readxl)
library(purrr)

out_dir <- file.path("data_proc", format(Sys.Date(), "result_%Y%m%d"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

colname_mapping <- read_excel("data_raw/colname_mapping.xlsx")

create_rename_vector <- function(mapping_df, year) {
  col_year <- paste0("col_", year)
  mapping_year <- mapping_df %>%
    filter(!is.na(.data[[col_year]])) %>%
    select(old_name = all_of(col_year), new_name = unified_name_en)
  setNames(mapping_year$old_name, mapping_year$new_name)
}

habit_vars <- c("reuse_bag", "energy_concern", "save_energy")
tpb_control_vars <- c("wil_of_engage", "category_trouble", "time_cost_troub")
years <- 2021:2023

ws_habit <- map2_dfr(
  paste0("data_raw/SHWS", years, ".xlsx"),
  years,
  function(file, year) {
    read_excel(file) %>%
      rename(any_of(create_rename_vector(colname_mapping, year))) %>%
      select(all_of(c(habit_vars, tpb_control_vars, "seper_recyc"))) %>%
      mutate(year = factor(year, levels = years), .before = 1)
  }
) %>%
  mutate(
    across(
      any_of(
        colname_mapping %>%
          filter(var_scale5_rev == 1) %>%
          pull(unified_name_en)
      ),
      ~ 6 - as.numeric(.x)
    )
  )

kendall_by_year <- function(data, predictor) {
  map_dfr(levels(data$year), function(y) {
    complete_data <- data %>%
      filter(year == y) %>%
      select(all_of(c(predictor, "seper_recyc"))) %>%
      mutate(across(everything(), as.numeric)) %>%
      tidyr::drop_na()

    test <- cor.test(
      complete_data[[predictor]], complete_data$seper_recyc,
      method = "kendall", exact = FALSE
    )

    tibble(
      year = y,
      habitual_behavior = predictor,
      kendall_tau = unname(test$estimate),
      p_value = test$p.value,
      n = nrow(complete_data)
    )
  })
}

cor_results <- map_dfr(habit_vars, ~ kendall_by_year(ws_habit, .x)) %>%
  mutate(
    significance = case_when(
      p_value < .001 ~ "***",
      p_value < .01  ~ "**",
      p_value < .05  ~ "*",
      TRUE ~ ""
    ),
    habitual_behavior = recode(
      habitual_behavior,
      reuse_bag      = "Reusable bag use",
      energy_concern = "Energy-efficiency concern",
      save_energy    = "Water and energy saving"
    ),
    habitual_behavior = factor(
      habitual_behavior,
      levels = c(
        "Reusable bag use",
        "Energy-efficiency concern",
        "Water and energy saving"
      )
    ),
    year = factor(year, levels = as.character(years)),
    cell_label = sprintf(
      "tau = %.3f%s\nn = %d", kendall_tau, significance, n
    )
  )

heatmap <- ggplot(
  cor_results,
  aes(x = year, y = habitual_behavior, fill = kendall_tau)
) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = cell_label), size = 3.5, lineheight = 1.05) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-0.30, 0.30),
    name = "Kendall's tau"
  ) +
  labs(
    title = "Habitual green behaviors vs. waste sorting",
    subtitle = "Kendall correlations by survey year; * p < .05, ** p < .01, *** p < .001",
    x = "Year", y = NULL
  ) +
  coord_fixed(ratio = 0.8) +
  theme_classic(base_size = 11) +
  theme(
    axis.ticks.y = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

write.csv(
  cor_results %>%
    select(
      year, habitual_behavior, kendall_tau,
      p_value, n, significance
    ),
  file.path(out_dir, "habitual_behavior_waste_sorting_correlations.csv"),
  row.names = FALSE
)
ggsave(
  file.path(out_dir, "habitual_behavior_waste_sorting_heatmap.pdf"),
  heatmap, width = 7.2, height = 3.8
)
ggsave(
  file.path(out_dir, "habitual_behavior_waste_sorting_heatmap.png"),
  heatmap, width = 7.2, height = 3.8, dpi = 300
)

print(cor_results %>% select(-cell_label))
cat("Outputs saved to:", out_dir, "\n")

# -----------------------------------------------------------------------------
# Simultaneous ordinal-logit models and within-year coefficient comparisons
# -----------------------------------------------------------------------------
# Predictors are standardized within year, so each coefficient is the change in
# cumulative log odds of reporting a higher waste-sorting category per 1-SD
# increase in that habitual behavior. All three behaviors enter simultaneously.

habit_labels <- c(
  reuse_bag      = "Reusable bag use",
  energy_concern = "Energy-efficiency concern",
  save_energy    = "Water and energy saving"
)

fit_habit_model <- function(data, year_value) {
  model_data <- data %>%
    filter(year == year_value) %>%
    select(all_of(c(habit_vars, "seper_recyc"))) %>%
    mutate(across(everything(), as.numeric)) %>%
    tidyr::drop_na() %>%
    mutate(
      across(all_of(habit_vars), ~ as.numeric(scale(.x))),
      seper_recyc = ordered(seper_recyc)
    )

  fit <- MASS::polr(
    seper_recyc ~ reuse_bag + energy_concern + save_energy,
    data = model_data,
    method = "logistic",
    Hess = TRUE
  )

  beta <- coef(fit)
  covariance <- vcov(fit)[names(beta), names(beta), drop = FALSE]
  standard_error <- sqrt(diag(covariance))
  z_value <- beta / standard_error

  coefficient_table <- tibble(
    year = year_value,
    habitual_behavior = names(beta),
    beta = unname(beta),
    se = unname(standard_error),
    ci_low = beta - qnorm(.975) * standard_error,
    ci_high = beta + qnorm(.975) * standard_error,
    odds_ratio = exp(beta),
    or_ci_low = exp(ci_low),
    or_ci_high = exp(ci_high),
    p_value = 2 * pnorm(-abs(z_value)),
    n = nrow(model_data),
    aic = AIC(fit)
  )

  behavior_pairs <- combn(names(beta), 2, simplify = FALSE)
  difference_table <- map_dfr(behavior_pairs, function(pair) {
    difference <- beta[pair[1]] - beta[pair[2]]
    difference_se <- sqrt(
      covariance[pair[1], pair[1]] + covariance[pair[2], pair[2]] -
        2 * covariance[pair[1], pair[2]]
    )
    z_difference <- difference / difference_se

    tibble(
      year = year_value,
      behavior_1 = pair[1],
      behavior_2 = pair[2],
      beta_difference = unname(difference),
      se_difference = unname(difference_se),
      ci_low = unname(difference - qnorm(.975) * difference_se),
      ci_high = unname(difference + qnorm(.975) * difference_se),
      z_value = unname(z_difference),
      p_value = 2 * pnorm(-abs(z_difference))
    )
  }) %>%
    mutate(
      p_holm = p.adjust(p_value, method = "holm"),
      significant_holm = p_holm < .05
    )

  list(fit = fit, coefficients = coefficient_table, differences = difference_table)
}

habit_models <- map(as.character(years), ~ fit_habit_model(ws_habit, .x))
names(habit_models) <- as.character(years)

model_coefficients <- map_dfr(habit_models, "coefficients") %>%
  mutate(
    habitual_behavior = recode(habitual_behavior, !!!habit_labels),
    habitual_behavior = factor(
      habitual_behavior,
      levels = unname(habit_labels)
    ),
    significance = case_when(
      p_value < .001 ~ "***",
      p_value < .01  ~ "**",
      p_value < .05  ~ "*",
      TRUE ~ ""
    )
  )

coefficient_differences <- map_dfr(habit_models, "differences") %>%
  mutate(
    behavior_1 = recode(behavior_1, !!!habit_labels),
    behavior_2 = recode(behavior_2, !!!habit_labels),
    comparison = paste(behavior_1, "minus", behavior_2),
    significance_holm = case_when(
      p_holm < .001 ~ "***",
      p_holm < .01  ~ "**",
      p_holm < .05  ~ "*",
      TRUE ~ ""
    )
  )

coefficient_plot <- ggplot(
  model_coefficients,
  aes(x = beta, y = habitual_behavior, color = factor(year))
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high),
    position = position_dodge(width = .55), width = .18, linewidth = .7,
    orientation = "y"
  ) +
  geom_point(position = position_dodge(width = .55), size = 2.8) +
  scale_color_manual(values = c("#4DBBD5", "#00A087", "#E64B35")) +
  labs(
    title = "Independent associations with waste-sorting behavior",
    subtitle = "Ordinal logit; all three standardized behaviors entered simultaneously",
    x = "Coefficient (log odds per 1-SD increase)", y = NULL, color = "Year"
  ) +
  theme_classic(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

difference_heatmap <- coefficient_differences %>%
  mutate(
    comparison = factor(comparison, levels = unique(comparison)),
    year = factor(year, levels = as.character(years)),
    cell_label = sprintf(
      "diff = %+.3f%s\nHolm p = %.3f",
      beta_difference, significance_holm, p_holm
    )
  ) %>%
  ggplot(aes(x = year, y = comparison, fill = beta_difference)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = cell_label), size = 3.1, lineheight = 1.05) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, name = "Coefficient\ndifference"
  ) +
  labs(
    title = "Pairwise differences between habitual-behavior coefficients",
    subtitle = "Positive values favor the first behavior; Holm-adjusted within each year",
    x = "Year", y = NULL
  ) +
  theme_classic(base_size = 10) +
  theme(axis.ticks.y = element_blank(), plot.title = element_text(face = "bold"))

write.csv(
  model_coefficients,
  file.path(out_dir, "habitual_behavior_ordinal_model_coefficients.csv"),
  row.names = FALSE
)
write.csv(
  coefficient_differences,
  file.path(out_dir, "habitual_behavior_coefficient_differences.csv"),
  row.names = FALSE
)
ggsave(
  file.path(out_dir, "habitual_behavior_ordinal_model_forest.pdf"),
  coefficient_plot, width = 7.2, height = 4.2
)
ggsave(
  file.path(out_dir, "habitual_behavior_ordinal_model_forest.png"),
  coefficient_plot, width = 7.2, height = 4.2, dpi = 300
)
ggsave(
  file.path(out_dir, "habitual_behavior_coefficient_difference_heatmap.pdf"),
  difference_heatmap, width = 8.5, height = 4.5
)
ggsave(
  file.path(out_dir, "habitual_behavior_coefficient_difference_heatmap.png"),
  difference_heatmap, width = 8.5, height = 4.5, dpi = 300
)

cat("\nSimultaneous ordinal-logit coefficients:\n")
print(model_coefficients)
cat("\nWithin-year coefficient comparisons (Holm-adjusted):\n")
print(coefficient_differences)

# -----------------------------------------------------------------------------
# Models adjusted for behavioral intention (BI) and perceived behavioral
# control (PBC)
# -----------------------------------------------------------------------------
# PBC is the mean of the same two items, with the same coding, used in the main
# PLS-SEM analysis. BI, PBC, and the three habitual behaviors are standardized
# within year. Pairwise tests concern only the three habitual-behavior
# coefficients.

fit_adjusted_habit_model <- function(data, year_value) {
  model_data <- data %>%
    filter(year == year_value) %>%
    select(all_of(c(habit_vars, tpb_control_vars, "seper_recyc"))) %>%
    mutate(across(everything(), as.numeric)) %>%
    tidyr::drop_na() %>%
    mutate(
      PBC = rowMeans(cbind(category_trouble, time_cost_troub)),
      across(all_of(c(habit_vars, "wil_of_engage", "PBC")),
             ~ as.numeric(scale(.x))),
      seper_recyc = ordered(seper_recyc)
    )

  fit <- MASS::polr(
    seper_recyc ~ reuse_bag + energy_concern + save_energy +
      wil_of_engage + PBC,
    data = model_data,
    method = "logistic",
    Hess = TRUE
  )

  beta <- coef(fit)
  covariance <- vcov(fit)[names(beta), names(beta), drop = FALSE]
  standard_error <- sqrt(diag(covariance))
  z_value <- beta / standard_error

  coefficient_table <- tibble(
    year = year_value,
    predictor = names(beta),
    beta = unname(beta),
    se = unname(standard_error),
    ci_low = beta - qnorm(.975) * standard_error,
    ci_high = beta + qnorm(.975) * standard_error,
    odds_ratio = exp(beta),
    or_ci_low = exp(ci_low),
    or_ci_high = exp(ci_high),
    p_value = 2 * pnorm(-abs(z_value)),
    n = nrow(model_data),
    aic = AIC(fit)
  )

  behavior_pairs <- combn(habit_vars, 2, simplify = FALSE)
  difference_table <- map_dfr(behavior_pairs, function(pair) {
    difference <- beta[pair[1]] - beta[pair[2]]
    difference_se <- sqrt(
      covariance[pair[1], pair[1]] + covariance[pair[2], pair[2]] -
        2 * covariance[pair[1], pair[2]]
    )
    z_difference <- difference / difference_se

    tibble(
      year = year_value,
      behavior_1 = pair[1],
      behavior_2 = pair[2],
      beta_difference = unname(difference),
      se_difference = unname(difference_se),
      ci_low = unname(difference - qnorm(.975) * difference_se),
      ci_high = unname(difference + qnorm(.975) * difference_se),
      z_value = unname(z_difference),
      p_value = 2 * pnorm(-abs(z_difference))
    )
  }) %>%
    mutate(
      p_holm = p.adjust(p_value, method = "holm"),
      significant_holm = p_holm < .05
    )

  list(fit = fit, coefficients = coefficient_table, differences = difference_table)
}

adjusted_models <- map(
  as.character(years),
  ~ fit_adjusted_habit_model(ws_habit, .x)
)
names(adjusted_models) <- as.character(years)

adjusted_coefficients <- map_dfr(adjusted_models, "coefficients") %>%
  mutate(
    predictor_label = recode(
      predictor, !!!habit_labels,
      wil_of_engage = "Behavioral intention",
      PBC = "Perceived behavioral control"
    ),
    predictor_type = if_else(
      predictor %in% habit_vars, "Habitual behavior", "TPB control"
    ),
    significance = case_when(
      p_value < .001 ~ "***",
      p_value < .01  ~ "**",
      p_value < .05  ~ "*",
      TRUE ~ ""
    )
  )

adjusted_differences <- map_dfr(adjusted_models, "differences") %>%
  mutate(
    behavior_1 = recode(behavior_1, !!!habit_labels),
    behavior_2 = recode(behavior_2, !!!habit_labels),
    comparison = paste(behavior_1, "minus", behavior_2),
    significance_holm = case_when(
      p_holm < .001 ~ "***",
      p_holm < .01  ~ "**",
      p_holm < .05  ~ "*",
      TRUE ~ ""
    )
  )

adjusted_forest_data <- adjusted_coefficients %>%
  filter(predictor_type == "Habitual behavior") %>%
  mutate(
    predictor_label = factor(predictor_label, levels = unname(habit_labels))
  )

adjusted_coefficient_plot <- ggplot(
  adjusted_forest_data,
  aes(x = beta, y = predictor_label, color = factor(year))
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high),
    position = position_dodge(width = .55), width = .18, linewidth = .7,
    orientation = "y"
  ) +
  geom_point(position = position_dodge(width = .55), size = 2.8) +
  scale_color_manual(values = c("#4DBBD5", "#00A087", "#E64B35")) +
  labs(
    title = "Habitual green behaviors adjusted for BI and PBC",
    subtitle = "Ordinal logit; standardized coefficients with 95% confidence intervals",
    x = "Adjusted coefficient (log odds per 1-SD increase)",
    y = NULL, color = "Year"
  ) +
  theme_classic(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

adjusted_difference_heatmap <- adjusted_differences %>%
  mutate(
    comparison = factor(comparison, levels = unique(comparison)),
    year = factor(year, levels = as.character(years)),
    cell_label = sprintf(
      "diff = %+.3f%s\nHolm p = %.3f",
      beta_difference, significance_holm, p_holm
    )
  ) %>%
  ggplot(aes(x = year, y = comparison, fill = beta_difference)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = cell_label), size = 3.1, lineheight = 1.05) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, name = "Coefficient\ndifference"
  ) +
  labs(
    title = "Adjusted differences between habitual-behavior coefficients",
    subtitle = "Controlling BI and PBC; Holm-adjusted within each year",
    x = "Year", y = NULL
  ) +
  theme_classic(base_size = 10) +
  theme(axis.ticks.y = element_blank(), plot.title = element_text(face = "bold"))

write.csv(
  adjusted_coefficients,
  file.path(out_dir, "habitual_behavior_adjusted_bi_pbc_coefficients.csv"),
  row.names = FALSE
)
write.csv(
  adjusted_differences,
  file.path(out_dir, "habitual_behavior_adjusted_bi_pbc_differences.csv"),
  row.names = FALSE
)
ggsave(
  file.path(out_dir, "habitual_behavior_adjusted_bi_pbc_forest.pdf"),
  adjusted_coefficient_plot, width = 7.2, height = 4.2
)
ggsave(
  file.path(out_dir, "habitual_behavior_adjusted_bi_pbc_forest.png"),
  adjusted_coefficient_plot, width = 7.2, height = 4.2, dpi = 300
)
ggsave(
  file.path(out_dir, "habitual_behavior_adjusted_bi_pbc_difference_heatmap.pdf"),
  adjusted_difference_heatmap, width = 8.5, height = 4.5
)
ggsave(
  file.path(out_dir, "habitual_behavior_adjusted_bi_pbc_difference_heatmap.png"),
  adjusted_difference_heatmap, width = 8.5, height = 4.5, dpi = 300
)

cat("\nOrdinal-logit coefficients adjusted for BI and PBC:\n")
print(adjusted_coefficients)
cat("\nAdjusted habitual-behavior coefficient comparisons:\n")
print(adjusted_differences)
