# Two-year landmark secondary heart-disease mortality curves by baseline UACR.
# The first two years are shown as pre-landmark follow-up; at year 2, follow-up
# restarts at zero among participants alive and uncensored at the landmark.
suppressPackageStartupMessages({
  library(survey); library(survival); library(ggplot2); library(cowplot); library(ragg)
})
options(warn = 1, survey.lonely.psu = "adjust")
FIGD <- "06_R_Code/supplementary_figure_generators/output"
dir.create(FIGD, recursive = TRUE, showWarnings = FALSE)

LANDMARK <- 2          # years
XMAX     <- 16.5       # common x-axis (years since baseline examination)
NAR_T    <- c(0, 2, 4, 8, 12, 16)

coh <- readRDS("07_R_Data/cohort_for_figures.rds")
coh <- subset(coh, bp_stage != "Treated (BP unmeasured)")
coh$bp_stage <- droplevels(coh$bp_stage)
coh$uacr_cat <- droplevels(coh$uacr_cat)

SHORT <- c("Normal (<10)", "Low-grade (10-29)", "Albuminuria (>=30)")
COLS  <- c("Normal (<10)" = "#3B6FB6",
           "Low-grade (10-29)" = "#E08E2A",
           "Albuminuria (>=30)" = "#B2182B")
coh$grp <- factor(SHORT[as.integer(coh$uacr_cat)], levels = SHORT)

ADJ <- "age_years + sex + race_eth + cycle_num + bmi_kgm2 + smoking_3cat + dm_composite + egfr_2021"

full_design <- svydesign(ids = ~psu, strata = ~strata, weights = ~wt_pooled,
                         nest = TRUE, data = coh)

coh$w_km_pre <- coh$wt_pooled / mean(coh$wt_pooled)
lm <- coh[coh$ftime_years > LANDMARK, , drop = FALSE]
lm$t_after <- lm$ftime_years - LANDMARK
lm$w_km_landmark <- lm$wt_pooled / mean(lm$wt_pooled)
des <- update(full_design, t_after = ftime_years - LANDMARK)
des <- subset(des, ftime_years > LANDMARK)

.tidy <- function(sf, g){
  lab <- gsub("^grp=", "", rep(names(sf$strata), sf$strata))
  d <- data.frame(time = sf$time, val = (1 - sf$surv) * 100,
                  lo = (1 - sf$upper) * 100, hi = (1 - sf$lower) * 100, grp = lab)
  d[d$grp == g, , drop = FALSE]
}
sf_pre <- survfit(Surv(ftime_years, death_hd) ~ grp, data = coh,
                  weights = w_km_pre, conf.type = "log-log")
sf_post <- survfit(Surv(t_after, death_hd) ~ grp, data = lm,
                   weights = w_km_landmark, conf.type = "log-log")

.piece <- function(sf, g, end, phase, shift = 0){
  d <- .tidy(sf, g)
  d <- d[d$time <= end, , drop = FALSE]
  d <- rbind(data.frame(time = 0, val = 0, lo = 0, hi = 0, grp = g), d)
  last <- d[nrow(d), , drop = FALSE]
  if (last$time < end) {
    last$time <- end
    d <- rbind(d, last)
  }
  d$time <- d$time + shift
  d$phase <- phase
  d
}

curve <- do.call(rbind, lapply(SHORT, function(g){
  rbind(.piece(sf_pre, g, LANDMARK, "Pre-landmark"),
        .piece(sf_post, g, XMAX - LANDMARK, "Post-landmark", LANDMARK))
}))
curve$grp <- factor(curve$grp, levels = SHORT)
curve$phase <- factor(curve$phase, levels = c("Pre-landmark", "Post-landmark"))

fit <- svycoxph(as.formula(paste("Surv(t_after, death_hd) ~ uacr_cat +", ADJ, "+ bp_stage")),
                design = des)
ci <- summary(fit)$conf.int
hr <- ci[grep("^uacr_cat", rownames(ci)), c(1, 3, 4), drop = FALSE]
hr_label <- sprintf(paste0("Adjusted landmark HRs vs UACR <10 mg/g:\n",
                           "  10-29 mg/g: %.2f (%.2f-%.2f)\n",
                           "  >=30 mg/g: %.2f (%.2f-%.2f)"),
                    hr[1, 1], hr[1, 2], hr[1, 3], hr[2, 1], hr[2, 2], hr[2, 3])

YMAX <- max(10, ceiling(max(curve$hi, na.rm = TRUE) / 2) * 2)

curve_panel <- ggplot() +
  annotate("rect", xmin = 0, xmax = LANDMARK, ymin = -Inf, ymax = Inf,
           fill = "grey94", alpha = 0.75) +
  geom_vline(xintercept = LANDMARK, linetype = "dashed",
             color = "grey55", linewidth = 0.4) +
  geom_ribbon(data = curve,
              aes(time, ymin = lo, ymax = hi, fill = grp,
                  group = interaction(grp, phase)),
              alpha = 0.14, colour = NA) +
  geom_step(data = curve,
            aes(time, val, color = grp, group = interaction(grp, phase)),
            linewidth = 0.95) +
  annotate("text", x = LANDMARK / 2, y = YMAX * 0.92,
           label = "Pre-landmark\nfollow-up", size = 2.8,
           color = "grey45", fontface = "italic") +
  annotate("label", x = LANDMARK + 0.25, y = YMAX * 0.78,
           hjust = 0, vjust = 1, size = 3.0,
           lineheight = 1.05, label = hr_label, fill = "white",
           label.r = unit(2, "pt")) +
  annotate("text", x = LANDMARK + 0.15, y = YMAX * 0.97, hjust = 0, vjust = 1,
           label = "2-year landmark\n(reset to 0)", size = 2.9,
           color = "grey45", fontface = "italic") +
  scale_color_manual(values = COLS, name = NULL) +
  scale_fill_manual(values = COLS, guide = "none") +
  scale_x_continuous(breaks = NAR_T, limits = c(0, XMAX),
                     expand = expansion(mult = c(0, 0), add = c(0, 0))) +
  scale_y_continuous(breaks = seq(0, YMAX, 2), limits = c(0, YMAX),
                     expand = expansion(mult = c(0, 0), add = c(0, 0))) +
  labs(x = NULL, y = "Cumulative heart-disease mortality (%)") +
  theme_classic(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.title.y = element_text(face = "bold"),
        legend.position = "top", legend.justification = "center",
        plot.margin = margin(8, 14, 2, 8))

nar <- expand.grid(t = NAR_T, grp = SHORT)
nar$n <- mapply(function(t, g) sum(coh$grp == g & coh$ftime_years >= t), nar$t, nar$grp)
nar$label_hjust <- ifelse(nar$t == 0, 0, 0.5)
nar$grp <- factor(nar$grp, levels = rev(SHORT))
risk_panel <- ggplot(nar, aes(t, grp, label = format(n, big.mark = ","),
                              hjust = label_hjust)) +
  geom_text(size = 2.9, color = "black") +
  scale_x_continuous(breaks = NAR_T, limits = c(0, XMAX),
                     expand = expansion(mult = c(0, 0), add = c(0, 0))) +
  coord_cartesian(clip = "off") +
  labs(x = "Years since baseline examination", y = NULL,
       subtitle = "No. at risk (unweighted participants)") +
  theme_classic(base_size = 12, base_family = "Helvetica") +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 9.5, color = "black", hjust = 0),
        axis.text.x = element_text(size = 9, color = "black"),
        axis.title.x = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic", color = "grey25"),
        plot.margin = margin(0, 14, 8, 8))

figure <- cowplot::plot_grid(curve_panel, risk_panel, ncol = 1,
                             rel_heights = c(3.6, 1), align = "v", axis = "lr")
ggsave(file.path(FIGD, "KM_2yrLandmark_UACR_HeartDisease.pdf"), figure,
       width = 10.0, height = 6.8, device = grDevices::pdf,
       family = "Helvetica", bg = "white")
ggsave(file.path(FIGD, "KM_2yrLandmark_UACR_HeartDisease.png"), figure,
       width = 10.0, height = 6.8, dpi = 300, device = ragg::agg_png, bg = "white")
ggsave(file.path(FIGD, "KM_2yrLandmark_UACR_HeartDisease.tiff"), figure,
       width = 10.0, height = 6.8, dpi = 600, device = ragg::agg_tiff,
       compression = "lzw", bg = "white")
cat("Heart-disease two-year landmark figure with pre-landmark shading and reset saved.\n")
