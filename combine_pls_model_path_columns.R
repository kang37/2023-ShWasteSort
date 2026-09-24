# Combine the three PLS model path-plot columns while preserving their scale.
# Column 3 is shorter and is aligned to the top of columns 1 and 2.

library(png)
library(grid)

source_dir <- file.path("data_proc", "result_20260921")
out_dir <- file.path("data_proc", "result_20260921")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
file_suffix <- Sys.getenv("PLS_COMBINE_SUFFIX")

source_files <- file.path(
  source_dir,
  paste0("pls_model_path_plot_col", 1:3, file_suffix, ".pdf")
)

stopifnot(all(file.exists(source_files)))

temp_dir <- tempfile("pls_model_columns_")
dir.create(temp_dir)
on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

png_files <- file.path(temp_dir, paste0("col", 1:3, ".png"))

for (i in seq_along(source_files)) {
  status <- system2(
    "pdftocairo",
    c("-png", "-singlefile", "-r", "300", source_files[i],
      sub("\\.png$", "", png_files[i]))
  )
  if (status != 0) stop("Failed to rasterize: ", source_files[i])
}

panels <- lapply(png_files, readPNG)

# Align the visible plot regions rather than the PDF page boundaries. Different
# numbers of facets can cause ggplot to leave different amounts of top margin.
first_ink_row <- function(x, threshold = .99) {
  rgb <- x[, , seq_len(min(3, dim(x)[3])), drop = FALSE]
  ink_by_row <- apply(rgb < threshold, 1, any)
  match(TRUE, ink_by_row)
}

top_rows <- vapply(panels, first_ink_row, integer(1))
panels <- Map(
  function(x, first_row) x[first_row:dim(x)[1], , , drop = FALSE],
  panels,
  top_rows
)

# Retain a small common margin after content alignment so the top strips do not
# touch the image boundary.
top_padding <- 40L
panels <- lapply(panels, function(x) {
  padded <- array(1, dim = c(top_padding + dim(x)[1], dim(x)[2], dim(x)[3]))
  padded[(top_padding + 1L):dim(padded)[1], , ] <- x
  padded
})

target_height <- max(vapply(panels, function(x) dim(x)[1], integer(1)))
target_channels <- max(vapply(panels, function(x) dim(x)[3], integer(1)))

normalize_channels <- function(x, channels) {
  if (dim(x)[3] == channels) return(x)
  if (dim(x)[3] == 3 && channels == 4) {
    alpha <- array(1, dim = c(dim(x)[1], dim(x)[2], 1))
    return(array(c(x, alpha), dim = c(dim(x)[1], dim(x)[2], 4)))
  }
  stop("Unexpected channel mismatch")
}

panels <- lapply(panels, normalize_channels, channels = target_channels)
panel_widths <- vapply(panels, function(x) dim(x)[2], integer(1))
combined <- array(
  1,
  dim = c(target_height, sum(panel_widths), target_channels)
)

x_start <- 1L
for (panel in panels) {
  panel_height <- dim(panel)[1]
  panel_width <- dim(panel)[2]
  x_end <- x_start + panel_width - 1L
  combined[seq_len(panel_height), x_start:x_end, ] <- panel
  x_start <- x_end + 1L
}

png_path <- file.path(out_dir, paste0("pls_model_path_plot_combined", file_suffix, ".png"))
pdf_path <- file.path(out_dir, paste0("pls_model_path_plot_combined", file_suffix, ".pdf"))

writePNG(combined, png_path)

pdf(
  pdf_path,
  width = sum(panel_widths) / 300,
  height = target_height / 300,
  bg = "white",
  useDingbats = FALSE
)
grid.newpage()
grid.raster(combined, width = unit(1, "npc"), height = unit(1, "npc"),
            interpolate = FALSE)
dev.off()

cat("Saved:", png_path, "\n")
cat("Saved:", pdf_path, "\n")
