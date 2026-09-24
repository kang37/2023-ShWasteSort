# Path trends for the base model and selected behavior-spillover model.
# Can be sourced by paper_output.R (using objects already in memory) or run
# independently using the latest saved spillover path table.

library(dplyr)
library(ggplot2)

if (!exists("all_spill_paths")) {
  input_path <- "data_proc/result_20260816/spillover_all_paths.csv"
  all_spill_paths <- read.csv(input_path, stringsAsFactors = FALSE)
}

if (!exists("out_dir")) {
  out_dir <- file.path("data_proc", format(Sys.Date(), "result_%Y%m%d"))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
}

selected_model_labels <- c(
  "M_base" = "Base model",
  "M_spill_direct" = "Behavior-spillover model"
)

selected_path_labels <- c(
  "DESC_NORM->ATT"             = "SN\n→ ATT",
  "DESC_NORM->PBC"             = "SN\n→ PBC",
  "PBC->ATT"                   = "PBC\n→ ATT",
  "DESC_NORM->wil_of_engage"   = "SN\n→ BI",
  "PBC->wil_of_engage"         = "PBC\n→ BI",
  "ATT->wil_of_engage"         = "ATT\n→ BI",
  "wil_of_engage->seper_recyc" = "BI\n→ BEH",
  "PBC->seper_recyc"           = "PBC\n→ BEH",
  "GreenBehav->seper_recyc"    = "HPEB\n→ BEH"
)

selected_path_colors <- c(
  "SN\n→ ATT"   = "#2166AC",
  "SN\n→ PBC"   = "#4393C3",
  "PBC\n→ ATT"  = "#92C5DE",
  "SN\n→ BI"    = "#1B7837",
  "PBC\n→ BI"   = "#5AAE61",
  "ATT\n→ BI"   = "#A6D96A",
  "BI\n→ BEH"   = "#D73027",
  "PBC\n→ BEH"  = "#F46D43",
  "HPEB\n→ BEH" = "#762A83"
)

selected_path_order <- names(selected_path_colors)

selected_models_plot_data <- all_spill_paths %>%
  filter(model %in% names(selected_model_labels)) %>%
  mutate(
    path_key = trimws(gsub("  ->  ", "->", path)),
    Path = factor(selected_path_labels[path_key], levels = selected_path_order),
    Model = factor(
      selected_model_labels[model],
      levels = unname(selected_model_labels)
    ),
    year_num = as.numeric(as.character(year)),
    significant = p_value < .05
  ) %>%
  filter(!is.na(Path))

selected_models_path_plot <- ggplot(
  selected_models_plot_data,
  aes(x = year_num, y = beta, color = Path, group = interaction(Model, Path))
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55", linewidth = .7) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high, fill = Path),
    alpha = .12, color = NA
  ) +
  geom_line(linewidth = 1.6) +
  geom_point(aes(shape = significant), size = 4.8, stroke = 1.6, fill = "white") +
  facet_grid(Model ~ Path, drop = FALSE) +
  scale_x_continuous(breaks = 2021:2023, limits = c(2020.65, 2023.35)) +
  scale_color_manual(values = selected_path_colors, guide = "none") +
  scale_fill_manual(values = selected_path_colors, guide = "none") +
  scale_shape_manual(
    values = c(`TRUE` = 16, `FALSE` = 21),
    labels = c(`TRUE` = "p < .05", `FALSE` = "Not significant"),
    name = NULL
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Year", y = "Standardized path\ncoefficient"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 18, face = "plain"),
    strip.background = element_rect(fill = "gray92", color = "gray60"),
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = .7),
    legend.position = "bottom",
    panel.spacing = grid::unit(.12, "lines")
  )

write.csv(
  selected_models_plot_data %>%
    select(year, model, Model, path, Path, beta, se, ci_low, ci_high, p_value, sig),
  file.path(out_dir, "selected_spillover_models_paths.csv"),
  row.names = FALSE
)
ggsave(
  file.path(out_dir, "selected_spillover_models_path_trends.pdf"),
  selected_models_path_plot, width = 28, height = 10
)
ggsave(
  file.path(out_dir, "selected_spillover_models_path_trends.png"),
  selected_models_path_plot, width = 28, height = 10, dpi = 300
)

cat("Saved: selected_spillover_models_path_trends.pdf / .png\n")
