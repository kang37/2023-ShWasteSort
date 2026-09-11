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
years <- 2021:2023

ws_habit <- map2_dfr(
  paste0("data_raw/SHWS", years, ".xlsx"),
  years,
  function(file, year) {
    read_excel(file) %>%
      rename(any_of(create_rename_vector(colname_mapping, year))) %>%
      select(all_of(c(habit_vars, "seper_recyc"))) %>%
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
