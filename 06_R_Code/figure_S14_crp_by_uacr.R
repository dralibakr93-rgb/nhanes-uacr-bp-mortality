# Supplementary Figure S9: adjusted geometric-mean CRP across UACR categories,
# shown for each blood-pressure-treatment group (exploratory).
#
# Design (replaces the earlier grouped-bar version): point-range SMALL MULTIPLES.
# For adjusted means, dot/point-range plots with 95% CIs are preferred over bars
# ("dynamite plots"), which imply a zero baseline that is not meaningful for a
# geometric mean and hide the data distribution (Weissgerber, PLoS Biol 2015).
# One small panel per BP-treatment group (Tufte small multiples) keeps each
# 3-point UACR gradient clean and un-crossed, so the reader sees the same
# monotonic rise repeated across every BP group (interaction p = 0.54) instead
# of overlapping spaghetti. A shared y-axis makes panels directly comparable.
#   * x-axis / colour = UACR category (the exposure gradient)
#   * y-axis          = adjusted geometric-mean CRP (mg/L) with 95% CI whiskers
#   * facet           = BP-treatment group
# Numbers are unchanged (from S14_crp_adjusted_geomean_figure.csv).

source("06_R_Code/00_config.R")
suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

save_supplement_figure <- function(plot, filename, width, height) {
  ggsave(file.path(paths$figures, paste0(filename, ".pdf")), plot,
         width = width, height = height, device = grDevices::pdf,
         family = "Helvetica", bg = "white")
  ggsave(file.path(paths$figures, paste0(filename, ".png")), plot,
         width = width, height = height, dpi = 300,
         device = ragg::agg_png, bg = "white")
  ggsave(file.path(paths$figures, paste0(filename, ".tiff")), plot,
         width = width, height = height, dpi = 600,
         device = ragg::agg_tiff, compression = "lzw", bg = "white")
}

cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
crp <- read.csv(file.path(paths$tables, "S14_crp_adjusted_geomean_figure.csv"),
                stringsAsFactors = FALSE)

# UACR category = x-axis AND colour (short labels; the exposure gradient).
uacr_short <- c("Normal", "Low-grade", "Albuminuria")
crp$uacr_category <- factor(crp$uacr_category, levels = levels(cohort$uacr_cat),
                            labels = uacr_short)
pal <- setNames(c("#1F4E79", "#E08E2A", "#C0392B"), uacr_short)   # matches S3

# BP-treatment group = facet (reader-friendly panel titles).
bp_lab <- c(
  "Non-elevated BP"        = "Non-elevated BP",
  "Elevated BP"            = "Elevated BP",
  "Stage 1 hypertension"   = "Untreated stage 1 HTN",
  "Stage 2 hypertension"   = "Untreated stage 2 HTN",
  "Treated - Controlled"   = "Treated, controlled HTN",
  "Treated - Uncontrolled" = "Treated, uncontrolled HTN"
)
bp_levels <- levels(cohort$bp_stage)
crp$bp_category <- factor(crp$bp_category, levels = bp_levels,
                          labels = bp_lab[bp_levels])

ypad  <- diff(range(c(crp$lower, crp$upper))) * 0.05
ylims <- c(min(crp$lower) - ypad, max(crp$upper) + ypad)

fig <- ggplot(crp, aes(uacr_category, adjusted_geometric_mean_mg_l,
                       color = uacr_category)) +
  geom_line(aes(group = 1), color = "grey60", linewidth = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.18, linewidth = 0.6) +
  geom_point(size = 2.6, shape = 16) +
  facet_wrap(~ bp_category, nrow = 2) +
  scale_color_manual(values = pal, name = "UACR category",
                     labels = c("Normal (<10 mg/g)",
                                "Low-grade (10-29 mg/g)",
                                "Albuminuria (>=30 mg/g)")) +
  scale_y_continuous(limits = ylims, breaks = seq(1.4, 2.8, by = 0.2),
                     labels = number_format(accuracy = 0.1),
                     expand = expansion(mult = c(0.02, 0.03))) +
  scale_x_discrete(expand = expansion(add = 0.55)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, y = "Adjusted geometric-mean CRP, mg/L"
  ) +
  theme_minimal(base_size = 12.5, base_family = "Helvetica") +
  theme(
    strip.text = element_text(face = "bold", size = 11.5, color = "grey15",
                              margin = margin(b = 4)),
    axis.title.y = element_text(face = "bold", size = 12, color = "grey15",
                                margin = margin(r = 8)),
    axis.text.x = element_text(size = 10, color = "grey20",
                               margin = margin(t = 3)),
    axis.text.y = element_text(size = 10.5, color = "grey20"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(0.9, "lines"),
    panel.spacing.y = unit(1.1, "lines"),
    axis.line = element_line(color = "grey45", linewidth = 0.4),
    axis.ticks = element_line(color = "grey45", linewidth = 0.35),
    axis.ticks.length = unit(0.1, "cm"),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(face = "bold", size = 11.5, color = "grey15"),
    legend.text = element_text(size = 11, color = "grey20"),
    legend.key.spacing.x = unit(0.4, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 16, 8, 10)
  )

save_supplement_figure(fig, "Supplementary_Figure_S9_CRP_by_UACR", 10, 6.8)
cat("Supplementary Figure S9 (small-multiples point-range) rebuilt.\n")
