# Figure 3: within-category absolute risk differences.
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
  "Low-grade albuminuria (10-29 mg/g)" = "Low-grade\n(10–29 mg/g)",
  "Albuminuria (>=30 mg/g)" = "Albuminuria\n(≥30 mg/g)"
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
# Figure 3: within-BP absolute risk differences.
within <- read.csv(file.path(paths$tables, "within_bp_contrasts.csv"),
                   stringsAsFactors = FALSE)
within$bp_label <- bp_labels[within$bp_category]
within$bp_label <- factor(within$bp_label, levels = rev(bp_labels[bp_order]))
within$comparison_group <- ifelse(
  grepl("Low-grade", within$comparison),
  "Low-grade (10–29) vs normal", "Albuminuria (≥30) vs normal"
)
within$comparison_group <- factor(
  within$comparison_group,
  levels = c("Low-grade (10–29) vs normal", "Albuminuria (≥30) vs normal")
)
within$y_base <- as.numeric(within$bp_label)
within$y <- within$y_base +
  ifelse(within$comparison_group == "Low-grade (10–29) vs normal", 0.20, -0.20)
within$ard_text <- sprintf("%+.1f (%.1f, %.1f)",
                           within$ARD_percentage_points,
                           within$ARD_lower, within$ARD_upper)
within$hr_text <- sprintf("%.2f (%.2f–%.2f)",
                          within$HR, within$HR_lower, within$HR_upper)
band_data <- data.frame(
  y = seq_along(levels(within$bp_label)),
  shade = seq_along(levels(within$bp_label)) %% 2 == 1
)
figure3 <- ggplot(within, aes(ARD_percentage_points, y,
                              color = comparison_group)) +
  geom_rect(data = subset(band_data, shade),
            aes(xmin = -0.6, xmax = 10.4, ymin = y - 0.5, ymax = y + 0.5),
            inherit.aes = FALSE, fill = "grey96", color = NA) +
  geom_vline(xintercept = 0, color = "grey35", linewidth = 0.7) +
  geom_vline(xintercept = seq(2, 10, by = 2), color = "grey87", linewidth = 0.5) +
  geom_errorbar(aes(xmin = ARD_lower, xmax = ARD_upper),
                orientation = "y", width = 0, linewidth = 1.0) +
  geom_point(size = 3.7) +
  geom_text(aes(x = 11.3, label = ard_text), hjust = 0, size = 4.0,
            color = "grey15") +
  geom_text(aes(x = 15.0, label = hr_text), hjust = 0, size = 4.0,
            color = "grey15") +
  annotate("text", x = 11.3, y = length(levels(within$bp_label)) + 0.72,
           label = "ARD, pp (95% CI)", hjust = 0, fontface = "bold",
           size = 4.2, color = "grey20") +
  annotate("text", x = 15.0, y = length(levels(within$bp_label)) + 0.72,
           label = "HR (95% CI)", hjust = 0, fontface = "bold",
           size = 4.2, color = "grey20") +
  scale_color_manual(
    values = c("Low-grade (10–29) vs normal" = "#E99225",
               "Albuminuria (≥30) vs normal" = "#C0392B"),
    name = NULL
  ) +
  scale_y_continuous(
    breaks = seq_along(levels(within$bp_label)),
    labels = levels(within$bp_label),
    limits = c(0.45, length(levels(within$bp_label)) + 0.82)
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 2), limits = c(-0.6, 18.0),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Adjusted 10-year absolute risk difference (percentage points)", y = NULL
  ) +
  theme_submission(base_size = 12.5) +
  theme(
    plot.title = element_text(size = 15.5, face = "bold"),
    plot.subtitle = element_text(size = 11.0),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12.2, color = "grey8"),
    axis.text.x = element_text(size = 11.2),
    legend.position = "bottom",
    legend.text = element_text(size = 11.5)
  )
save_figure(figure3, "Figure_3_WithinCategory_ARD", 11.4, 6.6)
