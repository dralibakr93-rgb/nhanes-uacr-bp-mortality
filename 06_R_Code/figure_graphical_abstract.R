# Graphical abstract: BP x UACR risk heatmap (same panel as Figure 2).
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")
suppressPackageStartupMessages({
  library(ggplot2)
  library(grid)
  library(scales)
})

cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
bp_order <- levels(cohort$bp_stage)
uacr_order <- levels(cohort$uacr_cat)

bp_labels <- c(
  "Non-elevated BP" = "Non-elevated BP",
  "Elevated BP" = "Elevated BP",
  "Stage 1 hypertension" = "Untreated stage 1 HTN",
  "Stage 2 hypertension" = "Untreated stage 2 HTN",
  "Treated - Controlled" = "Treated-controlled HTN",
  "Treated - Uncontrolled" = "Treated-uncontrolled HTN"
)

uacr_labels <- c(
  "Normal UACR (<10 mg/g)" = "Normal\n(<10 mg/g)",
  "Low-grade albuminuria (10-29 mg/g)" = "Low-grade\n(10-29 mg/g)",
  "Albuminuria (>=30 mg/g)" = "Albuminuria\n(>=30 mg/g)"
)

theme_submission <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", color = "grey8"),
      plot.subtitle = element_text(color = "grey35", margin = margin(b = 8)),
      axis.title = element_text(color = "grey8"),
      axis.text = element_text(color = "grey15"),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

save_figure <- function(plot, filename, width, height) {
  ggsave(file.path(paths$figures, paste0(filename, ".pdf")), plot,
         width = width, height = height, device = grDevices::pdf)
  ggsave(file.path(paths$figures, paste0(filename, ".png")), plot,
         width = width, height = height, dpi = 300,
         device = ragg::agg_png, bg = "white")
  ggsave(file.path(paths$figures, paste0(filename, ".tiff")), plot,
         width = width, height = height, dpi = 600,
         device = ragg::agg_tiff, compression = "lzw", bg = "white")
}

format_p <- function(value) {
  ifelse(value < 0.0001, "<0.0001", sprintf("%.4f", value))
}
# Figure 2: standardized 10-year risk heatmap.
joint <- read.csv(file.path(paths$tables, "joint_18_cells.csv"),
                  stringsAsFactors = FALSE)
joint$bp_category <- factor(joint$bp_category, levels = rev(bp_order),
                            labels = rev(bp_labels[bp_order]))
joint$uacr_category <- factor(joint$uacr_category, levels = uacr_order,
                              labels = uacr_labels[uacr_order])
joint$risk_label <- sprintf("%.1f%%", joint$adjusted_10y_risk_percent)
joint$n_label <- sprintf("n=%s; %s deaths",
                         format(joint$N, big.mark = ","),
                         format(joint$deaths, big.mark = ","))
joint$text_color <- ifelse(joint$adjusted_10y_risk_percent >= 11, "white", "grey8")
reference_cell <- joint[
  joint$bp_category == bp_labels["Non-elevated BP"] &
    grepl("^Normal", as.character(joint$uacr_category)),
]
figure2 <- ggplot(joint, aes(uacr_category, bp_category,
                             fill = adjusted_10y_risk_percent)) +
  geom_tile(color = "white", linewidth = 1.15) +
  geom_tile(data = reference_cell, fill = NA, color = "black", linewidth = 1.45) +
  geom_text(aes(y = as.numeric(bp_category) + 0.10,
                label = risk_label, color = text_color),
            fontface = "bold", size = 5.7) +
  geom_text(aes(y = as.numeric(bp_category) - 0.15,
                label = n_label, color = text_color),
            size = 3.15) +
  scale_color_identity() +
  scale_fill_gradient(
    low = "#FFF5EC", high = "#A93226",
    name = "Adjusted 10-yr\nrisk (%)", limits = c(4.5, 14.5),
    breaks = c(5, 8, 11, 14)
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title = "Adjusted 10-year all-cause mortality risk by BP-treatment category and UACR",
    subtitle = "Survey-weighted, marginally standardized complete-case Model 3; boxed cell = global reference.",
    x = "Urinary albumin-to-creatinine ratio (mg/g)", y = NULL
  ) +
  theme_submission(base_size = 12.5) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11.2),
    axis.text.x = element_text(size = 12.3, color = "grey8"),
    axis.text.y = element_text(size = 12.1, color = "grey8"),
    axis.title.x = element_text(size = 12.8, margin = margin(b = 4)),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )
save_figure(figure2, "Graphical_Abstract_BPxUACR", 10.8, 6.8)
