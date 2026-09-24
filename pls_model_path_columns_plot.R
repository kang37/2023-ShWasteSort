# Redraw the three base-PLS path columns from exported coefficients.
# This avoids rerunning bootstrap estimation when only figure styling changes.

library(dplyr)
library(ggplot2)
library(ggh4x)
library(grid)

input_file <- file.path("data_proc", "result_20260816", "pls_sem_path_coefficients.csv")
mga_file <- file.path("data_proc", "result_20260816", "pls_mga_all_pairs.csv")
out_dir <- file.path("data_proc", "result_20260921")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
add_mga_brackets <- identical(Sys.getenv("PLS_ADD_MGA"), "1")
file_suffix <- if (add_mga_brackets) "_mga" else ""

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

data_range <- range(c(plot_data$ci_low, plot_data$ci_high), na.rm = TRUE)
data_span <- diff(data_range)

mga_brackets <- data.frame()
if (add_mga_brackets) {
  path_tops <- plot_data %>%
    group_by(Path_full) %>%
    summarise(path_top = max(ci_high, na.rm = TRUE), .groups = "drop") %>%
    mutate(Path_full = as.character(Path_full))

  mga_brackets <- read.csv(mga_file, stringsAsFactors = FALSE) %>%
    filter(nzchar(sig)) %>%
    mutate(
      path_key = paste0(source, "->", target),
      Path_full = unname(path_labels[path_key]),
      year_a = as.numeric(year_a),
      year_b = as.numeric(year_b)
    ) %>%
    filter(!is.na(Path_full)) %>%
    left_join(path_tops, by = "Path_full") %>%
    group_by(Path_full) %>%
    arrange(year_a, year_b, .by_group = TRUE) %>%
    mutate(
      bracket_y = path_top + row_number() * .075 * data_span,
      bracket_end = bracket_y - .035 * data_span
    ) %>%
    ungroup()
}

global_ylim <- data_range + c(-.08, .08) * data_span
if (nrow(mga_brackets) > 0) {
  global_ylim[2] <- max(global_ylim[2], max(mga_brackets$bracket_y) + .04 * data_span)
}
height_unit <- 1.35
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

  bracket_data <- mga_brackets %>%
    filter(Path_full %in% paths) %>%
    mutate(Path_full = factor(Path_full, levels = paths))

  plot <- ggplot(d, aes(x = year_num, y = beta, color = Path_full)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = .4) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = Path_full),
                alpha = .12, color = NA) +
    geom_line(linewidth = .9) +
    geom_point(aes(fill = point_fill), shape = 21, size = 3.5, stroke = 1)

  if (nrow(bracket_data) > 0) {
    plot <- plot +
      geom_segment(
        data = bracket_data,
        aes(x = year_a, xend = year_b, y = bracket_y, yend = bracket_y),
        inherit.aes = FALSE, color = "black", linewidth = 1
      ) +
      geom_segment(
        data = bracket_data,
        aes(x = year_a, xend = year_a, y = bracket_y, yend = bracket_end),
        inherit.aes = FALSE, color = "black", linewidth = 1
      ) +
      geom_segment(
        data = bracket_data,
        aes(x = year_b, xend = year_b, y = bracket_y, yend = bracket_end),
        inherit.aes = FALSE, color = "black", linewidth = 1
      )
  }

  plot <- plot +
    facet_wrap(~ Path_full, ncol = 1, scales = "free_y") +
    facetted_pos_scales(y = y_scales) +
    force_panelsizes(rows = unit(rep(panel_height, n_panels), "in")) +
    scale_x_continuous(breaks = 2019:2023) +
    scale_color_manual(values = path_colors, guide = "none") +
    scale_fill_manual(values = fill_values, guide = "none") +
    labs(x = "Year", y = if (show_y_title) "Standardized Coefficient" else NULL) +
    theme_classic(base_size = 24) +
    theme(
      axis.text.x = element_text(size = 24, angle = 90),
      axis.text.y = element_text(size = 24),
      axis.title = element_text(size = 24),
      strip.text = element_text(size = 24),
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
    file.path(out_dir, paste0("pls_model_path_plot_col", i, file_suffix, ".pdf")),
    plot = columns[[i]]$plot,
    width = 6,
    height = columns[[i]]$height + 3.6
  )
}

cat("Saved updated PLS path plot columns", file_suffix, "to:", out_dir, "\n")
