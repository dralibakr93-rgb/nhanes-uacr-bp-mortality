# Supplementary Figure S12: decision-curve net benefit.
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")
suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

save_supplement_figure <- function(plot, filename, width, height) {
  ggsave(file.path(paths$figures, paste0(filename, ".pdf")), plot,
         width = width, height = height, device = grDevices::pdf)
  ggsave(file.path(paths$figures, paste0(filename, ".png")), plot,
         width = width, height = height, dpi = 300,
         device = ragg::agg_png, bg = "white")
  ggsave(file.path(paths$figures, paste0(filename, ".tiff")), plot,
         width = width, height = height, dpi = 600,
         device = ragg::agg_tiff, compression = "lzw", bg = "white")
}

theme_exploratory <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", color = "grey7"),
      plot.subtitle = element_text(color = "grey35", margin = margin(b = 10)),
      axis.title = element_text(face = "bold", color = "grey7"),
      axis.text = element_text(color = "grey18"),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

bp_labels <- c(
  "Non-elevated BP" = "Non-elevated BP",
  "Elevated BP" = "Elevated BP",
  "Stage 1 hypertension" = "Untreated stage 1 HTN",
  "Stage 2 hypertension" = "Untreated stage 2 HTN",
  "Treated - Controlled" = "Treated-controlled HTN",
  "Treated - Uncontrolled" = "Treated-uncontrolled HTN"
)
uacr_labels <- c(
  "Normal UACR (<10 mg/g)" = "Normal",
  "Low-grade albuminuria (10-29 mg/g)" = "Low-grade",
  "Albuminuria (>=30 mg/g)" = "Albuminuria"
)
uacr_profile_labels <- c(
  "Normal UACR (<10 mg/g)" = "Normal UACR (<10 mg/g)",
  "Low-grade albuminuria (10-29 mg/g)" = "Low-grade albuminuria (10-29 mg/g)",
  "Albuminuria (>=30 mg/g)" = "Albuminuria (>=30 mg/g)"
)
# Supplementary Figure S12: decision-curve analysis.
dca <- read.csv(file.path(paths$tables, "S12_decision_curve.csv"),
                stringsAsFactors = FALSE)
dca <- dca[dca$threshold <= 0.25, ]
dca$model <- factor(
  dca$model,
  levels = c(
    "Clinical model",
    "Clinical model + UACR (categorical)",
    "Clinical model + log2(UACR)",
    "Treat all",
    "Treat none"
  ),
  labels = c(
    "Clinical",
    "Clinical + UACR (categorical)",
    "Clinical + log2(UACR)",
    "Treat all",
    "Treat none"
  )
)
figure_s12 <- ggplot(dca, aes(threshold, net_benefit, color = model,
                              linetype = model)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.7) +
  geom_line(linewidth = 1.05) +
  scale_color_manual(
    values = c(
      "Clinical" = "#C0392B",
      "Clinical + UACR (categorical)" = "#1F4E79",
      "Clinical + log2(UACR)" = "#E99225",
      "Treat all" = "#2E86C1",
      "Treat none" = "grey55"
    ),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Clinical" = "solid",
      "Clinical + UACR (categorical)" = "solid",
      "Clinical + log2(UACR)" = "solid",
      "Treat all" = "solid",
      "Treat none" = "solid"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.25, by = 0.05),
    limits = c(0, 0.25)
  ) +
  scale_y_continuous(breaks = c(-0.10, -0.05, 0, 0.05)) +
  coord_cartesian(ylim = c(-0.13, 0.07)) +
  labs(
    title = "Decision-curve analysis: incremental net benefit of adding UACR",
    subtitle = "Full cohort (N=29,784); complete-case clinical model, 10-year all-cause mortality.",
    x = "Threshold probability (10-year mortality)", y = "Net benefit"
  ) +
  theme_exploratory(base_size = 13) +
  theme(
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 13.2),
    axis.text = element_text(size = 12)
  )
save_supplement_figure(
  figure_s12, "Supplementary_Figure_S12_DecisionCurve_NetBenefit", 10.2, 6.5
)
