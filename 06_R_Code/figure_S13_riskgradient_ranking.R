# Supplementary Figure S13: ranking of the 18 joint-risk cells.
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
# Supplementary Figure S13: ranking of the 18 adjusted joint-risk cells.
cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
joint <- read.csv(file.path(paths$tables, "joint_18_cells.csv"),
                  stringsAsFactors = FALSE)
joint$cell <- paste(
  bp_labels[joint$bp_category],
  uacr_labels[joint$uacr_category],
  sep = " + "
)
joint <- joint[order(joint$adjusted_10y_risk_percent), ]
joint$cell <- factor(joint$cell, levels = joint$cell)
figure_s13 <- ggplot(joint, aes(adjusted_10y_risk_percent, cell)) +
  geom_col(fill = "#4D7393", width = 0.70) +
  geom_text(aes(label = sprintf("%.1f%%", adjusted_10y_risk_percent)),
            hjust = -0.08, size = 4.2, color = "grey20") +
  scale_x_continuous(limits = c(0, 17.0), breaks = seq(0, 16, by = 4),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Adjusted 10-year all-cause mortality risk across BP-treatment × UACR cells",
    subtitle = "Survey-weighted, marginally standardized (complete-case Model 3); cells ranked low to high.",
    x = "Adjusted 10-year risk (%)", y = NULL
  ) +
  theme_exploratory(base_size = 12.5) +
  theme(
    plot.title = element_text(size = 15.2),
    plot.subtitle = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11.2, color = "grey8"),
    axis.title.x = element_text(size = 12.5),
    plot.margin = margin(8, 18, 8, 8)
  )
save_supplement_figure(
  figure_s13, "Supplementary_Figure_S13_RiskGradient_Ranking", 11.0, 7.6
)
