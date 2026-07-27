# Supplementary Figure S10: joint-risk profiles across UACR.
# Auto-organized from the verified pipeline; logic unchanged.

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

theme_exploratory <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
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
# Supplementary Figure S10: joint-risk profiles within BP categories.
profile <- joint
profile$bp_category <- factor(
  profile$bp_category, levels = levels(cohort$bp_stage),
  labels = bp_labels[levels(cohort$bp_stage)]
)
profile$uacr_category <- factor(
  profile$uacr_category, levels = levels(cohort$uacr_cat),
  labels = uacr_profile_labels[levels(cohort$uacr_cat)]
)
figure_s15 <- ggplot(
  profile,
  aes(uacr_category, adjusted_10y_risk_percent, group = bp_category,
      color = bp_category)
) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 3.0) +
  scale_color_manual(
    values = c(
      "Non-elevated BP" = "#F8766D",
      "Elevated BP" = "#B79F00",
      "Untreated stage 1 HTN" = "#00BA38",
      "Untreated stage 2 HTN" = "#00BFC4",
      "Treated-controlled HTN" = "#619CFF",
      "Treated-uncontrolled HTN" = "#F564E3"
    ),
    name = "BP category"
  ) +
  scale_y_continuous(breaks = seq(5, 15, by = 2.5), limits = c(4.4, 15.0)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "UACR category", y = "Adjusted 10-year risk (%)"
  ) +
  theme_exploratory(base_size = 12.5) +
  theme(
    plot.title = element_text(size = 16.5),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(angle = 22, hjust = 1, size = 11.5),
    axis.title = element_text(size = 12.5)
  )
save_supplement_figure(
  figure_s15, "Supplementary_Figure_S10_RiskProfile_BPxUACR", 10.2, 6.4
)
