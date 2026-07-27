# Figure 1: cohort-derivation flow diagram (publication CONSORT-style).
# Numbers come from cohort_flow_v2.csv (unchanged); only the layout/styling is set here.

source("06_R_Code/00_config.R")
suppressPackageStartupMessages({
  library(ggplot2)
  library(grid)
})

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

flow <- read.csv(file.path(paths$data, "cohort_flow_v2.csv"), stringsAsFactors = FALSE)
flow <- flow[flow$step != 7, ]
n <- flow$n
nb <- length(n)

main_lab <- c(
  "NHANES 1999-2018 participants",
  "Aged ≥30 years",
  "Valid UACR measured",
  "Eligible for mortality linkage",
  "Preserved eGFR ≥60 mL/min/1.73 m²",
  "No self-reported major cardiovascular disease",
  "Classifiable BP-treatment category (6 groups)",
  "Final analytic cohort"
)
excl_lab <- c(
  "Aged <30 years at examination",
  "No valid spot-urine UACR",
  "Not eligible for National Death Index linkage",
  "eGFR <60 mL/min/1.73 m² (2021 race-free CKD-EPI)",
  "Self-reported cardiovascular disease\n(CHD, MI, heart failure, angina, or stroke)",
  "Unclassifiable BP-treatment status\n(no valid BP, or missing BP/medication data)",
  "Pregnant at examination"
)
excl_n   <- n[-nb] - n[-1]
excl_pct <- 100 * excl_n / n[-nb]
fmt <- function(x) format(x, big.mark = ",", trim = TRUE)

# ---- layout geometry --------------------------------------------------------
pitch <- 1.5
yc    <- (nb - seq_len(nb)) * pitch      # box centres, top -> bottom
bh    <- 0.34                            # main box half-height
eh    <- 0.40                            # exclusion box half-height
xL <- 0.3;  xR <- 11.7;  xc <- (xL + xR) / 2
exL <- 13.0; exR <- 23.4; ex_txt <- exL + 0.35
vx <- xc                                 # vertical connector

col_fill <- "#EAF1F8"; col_edge <- "#2E4A63"; col_final <- "#1F4E79"
col_ex_fill <- "#F4F5F6"; col_ex_edge <- "#B7BFC6"; col_arrow <- "#2E4A63"

main_fill <- c(rep(col_fill, nb - 1), col_final)
main_edge <- c(rep(col_edge, nb - 1), col_final)
main_txt  <- c(rep("grey12", nb - 1), "white")
main_lwd  <- c(rep(1.2, nb - 1), 2.2)

main_df <- data.frame(x = xc, y = yc, lab = main_lab,
                      N = paste0("N = ", fmt(n)),
                      txt = main_txt, stringsAsFactors = FALSE)

ymid  <- yc[-nb] - pitch / 2
excl_df <- data.frame(
  y = ymid,
  head = sprintf("Excluded: N = %s (%.1f%%)", fmt(excl_n), excl_pct),
  reason = excl_lab, stringsAsFactors = FALSE
)

fig <- ggplot() +
  coord_cartesian(xlim = c(-0.4, 23.8), ylim = c(-0.9, pitch * (nb - 1) + 1.7),
                  clip = "off") +
  theme_void(base_family = "sans") +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(6, 8, 6, 8))

top_y <- pitch * (nb - 1)
fig <- fig +
  annotate("text", x = xL - 0.1, y = top_y + 1.5, hjust = 0, vjust = 1,
           label = "", fontface = "bold",
           size = 6.6, color = "grey10") +
  annotate("text", x = xL - 0.1, y = top_y + 1.03, hjust = 0, vjust = 1,
           label = "",
           size = 4.2, color = "grey40")

for (i in seq_len(nb)) {
  fig <- fig + annotation_custom(
    roundrectGrob(r = unit(0.16, "snpc"),
                  gp = gpar(fill = main_fill[i], col = main_edge[i], lwd = main_lwd[i])),
    xmin = xL, xmax = xR, ymin = yc[i] - bh, ymax = yc[i] + bh)
}
for (i in seq_len(nb - 1)) {
  fig <- fig + annotation_custom(
    roundrectGrob(r = unit(0.16, "snpc"),
                  gp = gpar(fill = col_ex_fill, col = col_ex_edge, lwd = 0.9)),
    xmin = exL, xmax = exR, ymin = ymid[i] - eh, ymax = ymid[i] + eh)
}

fig <- fig +
  geom_segment(data = data.frame(y = yc[-nb] - bh, yend = yc[-1] + bh),
               aes(x = vx, xend = vx, y = y, yend = yend),
               arrow = arrow(length = unit(0.17, "cm"), type = "closed"),
               linewidth = 0.6, color = col_arrow, lineend = "round") +
  geom_segment(data = excl_df, aes(x = vx, xend = exL, y = y, yend = y),
               arrow = arrow(length = unit(0.14, "cm"), type = "closed"),
               linewidth = 0.5, color = "#9AA5AE") +
  geom_text(data = main_df, aes(x = x, y = y + 0.10, label = lab, color = txt),
            fontface = "bold", size = 4.15, lineheight = 0.9) +
  geom_text(data = main_df, aes(x = x, y = y - 0.16, label = N, color = txt),
            size = 4.35) +
  geom_text(data = excl_df, aes(x = ex_txt, y = y + 0.16, label = head),
            hjust = 0, fontface = "bold", size = 3.5, color = "grey15") +
  geom_text(data = excl_df, aes(x = ex_txt, y = y - 0.12, label = reason),
            hjust = 0, size = 3.4, color = "grey30", lineheight = 0.92) +
  scale_color_identity()

save_figure(fig, "Figure_1_Cohort_Derivation", 9.8, 12.4)
cat("Figure 1 rebuilt.\n")
