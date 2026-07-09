# ============================================================================
# Spillover Model Path Diagram  (revised)
# Left panel  : structural path diagram (M_spill_full, mean beta 2021-2023)
# Right panels: year-by-year β with 95% CI + mediation indirect effect
# ============================================================================
pacman::p_load(dplyr, ggplot2, patchwork, showtext)
showtext::showtext_auto()

# ── colours ──────────────────────────────────────────────────────────────────
col_tpb     <- "#2166AC"
col_spill_d <- "#C0392B"
col_spill_a <- "#27AE60"
col_med     <- "#8E44AD"
col_ns      <- "gray60"
col_bord    <- "gray35"

# ── data ─────────────────────────────────────────────────────────────────────
all_paths <- read.csv("data_proc/spillover_all_paths.csv")
mediation  <- read.csv("data_proc/spillover_mediation.csv")
key_paths  <- read.csv("data_proc/spillover_key_paths.csv")

mean_full <- all_paths %>%
  filter(model == "M_spill_full") %>%
  mutate(path = trimws(gsub("  ->  ", "->", path))) %>%
  group_by(path) %>%
  summarise(beta = mean(beta), .groups = "drop")

gb <- function(from, to) {
  pat <- paste0(from, "->", to)
  round(mean_full$beta[mean_full$path == pat], 2)
}

b_inj  <- gb("INJ_NORM",      "ATT")           # 0.73
b_desc <- gb("DESC_NORM",     "ATT")            # 0.00
b_pbc  <- gb("PBC",           "ATT")            # 0.35
b_gba  <- gb("GreenBehav",    "ATT")            # 0.14
b_gbb  <- gb("GreenBehav",    "seper_recyc")    # 0.20
b_att  <- gb("ATT",           "wil_of_engage")  # 0.73
b_bi   <- gb("wil_of_engage", "seper_recyc")    # 0.26

fmt_b <- function(b) sprintf("b = %+.2f", b)

# ── canvas helpers ─────────────────────────────────────────────────────────
# node: rounded rectangle + two-line label
node <- function(cx, cy, w=1.9, h=0.72,
                 top, bot=NULL,
                 fill="white", bord=col_bord, bsz=0.55) {
  out <- list(
    annotate("rect",
      xmin=cx-w/2, xmax=cx+w/2,
      ymin=cy-h/2, ymax=cy+h/2,
      fill=fill, color=bord, linewidth=bsz)
  )
  if (is.null(bot)) {
    out <- c(out, list(
      annotate("text", x=cx, y=cy, label=top,
               size=4.2, fontface="bold", hjust=0.5, vjust=0.5)
    ))
  } else {
    out <- c(out, list(
      annotate("text", x=cx, y=cy+0.15, label=top,
               size=4.2, fontface="bold", hjust=0.5, vjust=0.5),
      annotate("text", x=cx, y=cy-0.17, label=bot,
               size=3.1, color="gray40", hjust=0.5, vjust=0.5)
    ))
  }
  out
}

# directed arrow
arr <- function(x0,y0,x1,y1, col, lwd=1.15, lty="solid",
                shrink=0.03) {
  dx <- x1-x0; dy <- y1-y0
  list(annotate("segment",
    x    = x0 + shrink*dx,  y    = y0 + shrink*dy,
    xend = x1 - shrink*dx,  yend = y1 - shrink*dy,
    color=col, linewidth=lwd, linetype=lty,
    arrow=arrow(length=unit(7,"pt"), type="closed")))
}

# text on arrow
alab <- function(x, y, txt, col="gray20", sz=3.8, bold=FALSE) {
  list(annotate("text", x=x, y=y, label=txt,
                color=col, size=sz,
                fontface=if(bold) "bold" else "plain",
                hjust=0.5, vjust=0.5))
}

# ── node positions ────────────────────────────────────────────────────────
xG=0;  yG= 2.80   # GreenBehav
xI=0;  yI= 1.55   # INJ_NORM
xD=0;  yD= 0.55   # DESC_NORM
xP=0;  yP=-0.45   # PBC
xA=3.4; yA=0.80   # ATT
xB=5.9; yB=0.80   # BI
xE=8.4; yE=0.80   # BEH

panel_a <- ggplot() +

  # ── TPB core arrows ──────────────────────────────────────────────────────
  arr(xI,yI, xA,yA, col=col_tpb, lwd=1.5) +
  alab(1.6, 1.38, fmt_b(b_inj), col=col_tpb, bold=TRUE) +

  arr(xD,yD, xA,yA, col=col_ns, lwd=0.8, lty="dashed") +
  alab(1.6, 0.68, fmt_b(b_desc), col=col_ns) +

  arr(xP,yP, xA,yA, col=col_tpb, lwd=1.2) +
  alab(1.6, 0.08, fmt_b(b_pbc), col=col_tpb, bold=TRUE) +

  arr(xA,yA, xB,yB, col=col_tpb, lwd=1.7) +
  alab(4.65, 0.98, fmt_b(b_att), col=col_tpb, bold=TRUE) +

  arr(xB,yB, xE,yE, col=col_tpb, lwd=1.4) +
  alab(7.15, 0.98, fmt_b(b_bi), col=col_tpb, bold=TRUE) +

  # ── Layer 3: GreenBehav -> ATT (green) ───────────────────────────────────
  arr(xG+0.05, yG-0.33, xA-0.05, yA+0.33,
      col=col_spill_a, lwd=1.3) +
  alab(1.65, 2.05, fmt_b(b_gba), col=col_spill_a, bold=TRUE) +

  # ── Layer 2: GreenBehav -> seper_recyc (red arc) ─────────────────────────
  annotate("curve",
    x    = xG+0.95, y = yG+0.00,
    xend = xE-0.10, yend = yE+0.33,
    curvature=-0.30,
    color=col_spill_d, linewidth=1.5,
    arrow=arrow(length=unit(6,"pt"), type="closed")) +
  alab(4.30, 3.20, fmt_b(b_gbb), col=col_spill_d, bold=TRUE, sz=4.0) +

  # ── nodes ─────────────────────────────────────────────────────────────────
  node(xG, yG, top="GreenBehav",
       bot="reuse_bag  energy_concern  save_energy",
       fill="#FFFBEF", bord="#D68910", bsz=0.7) +
  node(xI, yI, top="INJ_NORM",
       bot="pr_atten  regulate_law  regulate_commu_rule",
       fill="#EEF4FF") +
  node(xD, yD, top="DESC_NORM",
       bot="if_neighbor_ws  if_family_ws",
       fill="#F7F7F7", bord="gray60") +
  node(xP, yP, top="PBC",
       bot="category_trouble  time_cost_troub",
       fill="#EEF4FF") +
  node(xA, yA, top="ATT",
       bot="ws_attitude  ws_interest  threat",
       fill="#EEF4FF") +
  node(xB, yB, top="Intention (BI)",
       bot="wil_of_engage",
       fill="#EEF4FF") +
  node(xE, yE, top="Behavior",
       bot="seper_recyc",
       fill="#EDFAEE") +

  # ── legend ────────────────────────────────────────────────────────────────
  # row 1
  annotate("segment", x=0.2,xend=0.9, y=-1.55,yend=-1.55,
           color=col_tpb, linewidth=1.4,
           arrow=arrow(length=unit(5,"pt"),type="closed")) +
  annotate("text", x=1.0,y=-1.55, hjust=0, size=3.5, color=col_tpb,
           label="TPB core path (M7 baseline)") +
  annotate("segment", x=4.2,xend=4.9, y=-1.55,yend=-1.55,
           color=col_spill_d, linewidth=1.4,
           arrow=arrow(length=unit(5,"pt"),type="closed")) +
  annotate("text", x=5.0,y=-1.55, hjust=0, size=3.5, color=col_spill_d,
           label="Layer 2: direct spillover  GreenBehav -> BEH") +
  # row 2
  annotate("segment", x=0.2,xend=0.9, y=-1.95,yend=-1.95,
           color=col_spill_a, linewidth=1.4,
           arrow=arrow(length=unit(5,"pt"),type="closed")) +
  annotate("text", x=1.0,y=-1.95, hjust=0, size=3.5, color=col_spill_a,
           label="Layer 3: attitudinal spillover  GreenBehav -> ATT") +
  annotate("segment", x=4.2,xend=4.9, y=-1.95,yend=-1.95,
           color=col_ns, linewidth=0.9, linetype="dashed",
           arrow=arrow(length=unit(5,"pt"),type="closed")) +
  annotate("text", x=5.0,y=-1.95, hjust=0, size=3.5, color=col_ns,
           label="Non-significant path") +
  # footnote
  annotate("text", x=4.3, y=-2.38, hjust=0.5, size=3.0,
           color="gray55", fontface="italic",
           label="b = mean standardised coefficient, 2021-2023  (M_spill_full, bootstrap n = 1000)") +

  coord_fixed(ratio=1, xlim=c(-1.3,9.7), ylim=c(-2.5,3.5)) +
  theme_void() +
  theme(plot.margin=margin(6,4,6,6))

# ── Panel B: year-by-year spillover paths ────────────────────────────────────
pdat <- key_paths %>%
  filter(model == "M_spill_full") %>%
  mutate(
    layer = case_when(
      grepl("seper_recyc", path) ~ "Layer 2: GreenBehav -> Behavior (direct)",
      grepl("ATT",         path) ~ "Layer 3: GreenBehav -> Attitude"
    ),
    layer    = factor(layer, levels=c(
      "Layer 3: GreenBehav -> Attitude",
      "Layer 2: GreenBehav -> Behavior (direct)"
    )),
    year_num = as.numeric(year),
    col_grp  = ifelse(grepl("direct", layer),
                      "Layer 2: GreenBehav -> Behavior (direct)",
                      "Layer 3: GreenBehav -> Attitude")
  )

lcols <- c(
  "Layer 2: GreenBehav -> Behavior (direct)" = col_spill_d,
  "Layer 3: GreenBehav -> Attitude"           = col_spill_a
)

panel_b <- ggplot(pdat, aes(x=year_num, y=beta,
                             color=col_grp, fill=col_grp, group=col_grp)) +
  geom_hline(yintercept=0, linetype="dashed", color="gray55", linewidth=0.4) +
  geom_ribbon(aes(ymin=ci_low, ymax=ci_high), alpha=0.14, color=NA) +
  geom_line(linewidth=1.1) +
  geom_point(size=3.5, shape=21, color="white", stroke=1.3) +
  geom_text(aes(label=paste0(sprintf("%.2f", beta), sig)),
            vjust=-1.3, size=3.0, show.legend=FALSE) +
  facet_wrap(~layer, ncol=1, scales="free_y") +
  scale_x_continuous(breaks=2021:2023) +
  scale_y_continuous(expand=expansion(mult=c(0.15, 0.28))) +
  scale_color_manual(values=lcols, guide="none") +
  scale_fill_manual( values=lcols, guide="none") +
  labs(x=NULL, y="Standardised b  (95% CI)") +
  theme_classic(base_size=10) +
  theme(
    strip.background = element_rect(fill="gray93", color="gray65"),
    strip.text       = element_text(size=9, face="bold"),
    panel.border     = element_rect(color="gray65", fill=NA, linewidth=0.4),
    axis.text        = element_text(size=9),
    plot.margin      = margin(4,10,4,4)
  )

# ── Panel C: mediation indirect effect ───────────────────────────────────────
med_dat <- mediation %>%
  filter(grepl("BEH", path)) %>%
  mutate(year_num=as.numeric(year))

panel_c <- ggplot(med_dat, aes(x=year_num, y=beta)) +
  geom_hline(yintercept=0, linetype="dashed", color="gray55", linewidth=0.4) +
  geom_ribbon(aes(ymin=ci_low, ymax=ci_high), fill=col_med, alpha=0.17) +
  geom_line(color=col_med, linewidth=1.1) +
  geom_point(size=3.5, shape=21, fill=col_med, color="white", stroke=1.3) +
  geom_text(aes(label=paste0(sprintf("%.3f", beta), sig)),
            vjust=-1.1, size=2.75, color=col_med) +
  scale_x_continuous(breaks=2021:2023) +
  scale_y_continuous(expand=expansion(mult=c(0.15, 0.30))) +
  labs(
    title = "Mediation: GreenBehav -> ATT -> BI -> BEH (indirect)",
    x=NULL, y="Indirect b  (bootstrapped)"
  ) +
  theme_classic(base_size=10) +
  theme(
    plot.title   = element_text(size=8.8, face="bold", hjust=0.5, color=col_med),
    panel.border = element_rect(color="gray65", fill=NA, linewidth=0.4),
    axis.text    = element_text(size=9),
    plot.margin  = margin(6,10,4,4)
  )

# ── Assemble ─────────────────────────────────────────────────────────────────
right_col  <- (panel_b / panel_c) + plot_layout(heights=c(2.2, 1))
final_plot <- (panel_a | right_col) +
  plot_layout(widths=c(1.75, 1)) +
  plot_annotation(
    title    = "Spillover Effects of Pro-environmental Behaviour on Waste Sorting",
    subtitle = "Layer 2 (direct) and Layer 3 (attitudinal + mediated)  |  M_spill_full, Shanghai 2021-2023",
    theme=theme(
      plot.title    = element_text(size=11.5, face="bold", hjust=0.5,
                                   margin=margin(b=2)),
      plot.subtitle = element_text(size=8.5, color="gray45", hjust=0.5,
                                   margin=margin(b=6))
    )
  )

ggsave("data_proc/spillover_diagram.pdf", final_plot,
       width=18, height=9, device=cairo_pdf)
ggsave("data_proc/spillover_diagram.png", final_plot,
       width=18, height=9, dpi=180)
cat("Saved: data_proc/spillover_diagram.pdf / .png\n")
