# Redraw the three base-PLS path columns from exported coefficients.
# This avoids rerunning bootstrap estimation when only figure styling changes.

library(dplyr)
library(ggplot2)
library(ggh4x)
library(grid)

input_file <- file.path("data_proc", "result_20260816", "pls_sem_path_coefficients.csv")
out_dir <- file.path("data_proc", "result_20260921")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

path_results <- read.csv(input_file, stringsAsFactors = FALSE)

path_labels <- c(
  "DESC_NORM->wil_of_engage"   = "Subjective norm → Intention",
  "PBC->wil_of_engage"         = "PBC → Intention",
  "ATT->wil_of_engage"         = "Attitude → Intention",
  "wil_of_engage->seper_recyc" = "Intention → Behavior",
  "PBC->seper_recyc"           = "PBC → Behavior",
  "DESC_NORM->ATT"             = "Subjective norm → Attitude",
  "DESC_NORM->PBC"             = "Subjective norm → PBC",
  "PBC->ATT"                   = "PBC → Attitude"
)

path_colors <- c(
  "Subjective norm → Intention" = "#1B7837",
  "PBC → Intention"             = "#5AAE61",
  "Attitude → Intention"        = "#A6D96A",
  "Intention → Behavior"        = "#D73027",
  "PBC → Behavior"              = "#F46D43",
  "Subjective norm → Attitude"  = "#2166AC",
  "Subjective norm → PBC"       = "#4393C3",
  "PBC → Attitude"              = "#92C5DE"
)

plot_data <- path_results %>%
  mutate(
    path_key = paste0(rhs, "->", lhs),
    Path_full = path_labels[path_key],
    year_num = as.numeric(year),
    significant = p_value < .05
  ) %>%
  filter(!is.na(Path_full)) %>%
  mutate(
    Path_full = factor(Path_full, levels = names(path_colors)),
    point_fill = if_else(significant, as.character(Path_full), "ns")
  )

global_ylim <- range(c(plot_data$ci_low, plot_data$ci_high), na.rm = TRUE)
global_pad <- diff(global_ylim) * .08
global_ylim <- global_ylim + c(-global_pad, global_pad)
height_unit <- 1.6
panel_height <- diff(global_ylim) * height_unit
fill_values <- c(path_colors, ns = "white")

make_path_col <- function(paths, show_y_title = TRUE) {
  d <- plot_data %>%
    filter(Path_full %in% paths) %>%
    mutate(Path_full = factor(Path_full, levels = paths))
  n_panels <- length(paths)
  breaks_y <- pretty(global_ylim, n = 5)
  breaks_y <- breaks_y[breaks_y >= global_ylim[1] & breaks_y <= global_ylim[2]]
  y_scales <- rep(
    list(scale_y_continuous(limits = global_ylim, breaks = breaks_y)),
    n_panels
  )

  plot <- ggplot(d, aes(x = year_num, y = beta, color = Path_full)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = .4) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = Path_full),
                alpha = .12, color = NA) +
    geom_line(linewidth = .9) +
    geom_point(aes(fill = point_fill), shape = 21, size = 3.5, stroke = 1) +
    facet_wrap(~ Path_full, ncol = 1, scales = "free_y") +
    facetted_pos_scales(y = y_scales) +
    force_panelsizes(rows = unit(rep(panel_height, n_panels), "in")) +
    scale_x_continuous(breaks = 2019:2023) +
    scale_color_manual(values = path_colors, guide = "none") +
    scale_fill_manual(values = fill_values, guide = "none") +
    labs(x = "Year", y = if (show_y_title) "Standardized Coefficient" else NULL) +
    theme_classic(base_size = 18) +
    theme(
      axis.text.x = element_text(angle = 90),
      strip.text = element_text(size = 18),
      strip.background = element_rect(fill = "gray85", color = "gray50"),
      panel.border = element_rect(color = "gray50", fill = NA, linewidth = .5)
    )

  list(plot = plot, height = n_panels * panel_height)
}

columns <- list(
  make_path_col(c("Subjective norm → Attitude", "Subjective norm → PBC", "PBC → Attitude")),
  make_path_col(c("Subjective norm → Intention", "PBC → Intention", "Attitude → Intention"), FALSE),
  make_path_col(c("Intention → Behavior", "PBC → Behavior"), FALSE)
)

for (i in seq_along(columns)) {
  ggsave(
    file.path(out_dir, paste0("pls_model_path_plot_col", i, ".pdf")),
    plot = columns[[i]]$plot,
    width = 5,
    height = columns[[i]]$height + 3
  )
}

cat("Saved updated PLS path plot columns to:", out_dir, "\n")
