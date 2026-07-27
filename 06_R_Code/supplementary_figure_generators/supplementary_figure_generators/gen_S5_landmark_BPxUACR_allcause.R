# Summary: Computes and draws the 2-year landmark all-cause analysis by UACR within the six BP categories (BP-panel figure).
# =============================================================================
# 45_landmark2yr_bpxuacr.R
# Two-year landmark, all-cause mortality, by UACR category — overall + the six
# BP-treatment categories, on the rebuilt full-spectrum primary cohort.
# -----------------------------------------------------------------------------
# Landmark design : exclude anyone who died/was censored before 2 yr; reset
#                   follow-up to zero at the 2-yr landmark (t_after = ftime-2).
# Curves          : survey-weighted Kaplan-Meier failure probability via svykm
#                   (DESCRIPTIVE — complex design in the point estimate).
# Inference       : survey-weighted adjusted landmark Cox (Model 3 covariates);
#                   HRs shown in each panel. These are the inferential result.
# Layout          : Panels A-F = the six BP-treatment categories (3 UACR curves
#                   each). Number-at-risk per panel.
# Placement       : SUPPLEMENT.
# Output          : Supplementary_Figure_Landmark2yr_BPxUACR.{tiff,png,pdf}
# =============================================================================

suppressPackageStartupMessages({
  library(survey); library(survival); library(ggplot2); library(cowplot); library(ragg)
})
options(warn = 1, survey.lonely.psu = "adjust")
OUTDIR <- "06_R_Code/supplementary_figure_generators/output"
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)

LANDMARK <- 2          # years
XMAX     <- 16.5       # common x-axis (years since baseline examination)
NAR_T    <- c(0, 2, 4, 8, 12, 16)

coh <- readRDS("07_R_Data/cohort_for_figures.rds")
coh <- subset(coh, bp_stage != "Treated (BP unmeasured)")
coh$bp_stage <- droplevels(coh$bp_stage)
coh$uacr_cat <- droplevels(coh$uacr_cat)

# Short UACR labels + colorblind-safe colors (Normal blue / Low-grade amber / Albuminuria red)
SHORT <- c("UACR <10", "UACR 10-29", "UACR >=30")
COLS  <- c("UACR <10" = "#3B6FB6", "UACR 10-29" = "#E08E2A", "UACR >=30" = "#B2182B")
coh$grp <- factor(SHORT[as.integer(coh$uacr_cat)], levels = SHORT)
full_design <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~wt_pooled,
  nest = TRUE, data = coh
)

ADJ   <- "age_years + sex + race_eth + cycle_num + bmi_kgm2 + smoking_3cat + dm_composite + egfr_2021"
BPS   <- levels(coh$bp_stage)
PANELS <- c("Overall (six BP categories combined)", BPS)
LETT  <- LETTERS[seq_along(PANELS)]
BP_DISPLAY <- c(
  "Non-elevated BP" = "Non-elevated BP",
  "Elevated BP" = "Elevated BP",
  "Stage 1 hypertension" = "Untreated stage 1 hypertension",
  "Stage 2 hypertension" = "Untreated stage 2 hypertension",
  "Treated - Controlled" = "Treated-controlled hypertension",
  "Treated - Uncontrolled" = "Treated-uncontrolled hypertension"
)

verif <- data.frame()

# ---- single panel (curve over number-at-risk strip) -------------------------
make_panel <- function(idx) {
  ttl_stub <- PANELS[idx]; is_overall <- idx == 1
  sub <- if (is_overall) coh else coh[coh$bp_stage == ttl_stub, , drop = FALSE]
  sub$grp <- factor(SHORT[as.integer(sub$uacr_cat)], levels = SHORT)
  des_full <- if (is_overall) full_design else subset(full_design, bp_stage == ttl_stub)

  # Landmark: alive & uncensored at LANDMARK, reset clock
  lm <- sub[sub$ftime_years > LANDMARK, , drop = FALSE]
  lm$t_after <- lm$ftime_years - LANDMARK
  fac <- vapply(lm, is.factor, logical(1)); lm[fac] <- lapply(lm[fac], droplevels)
  lm$grp <- factor(SHORT[as.integer(lm$uacr_cat)], levels = SHORT)

  des <- update(des_full, t_after = ftime_years - LANDMARK)
  des <- subset(des, ftime_years > LANDMARK)

  # Show the first two years as pre-landmark follow-up, then restart the curves
  # at zero among two-year survivors. The adjusted Cox model below uses the
  # same post-landmark population.
  sub$w_km_pre <- sub$wt_pooled / mean(sub$wt_pooled)
  lm$w_km_landmark <- lm$wt_pooled / mean(lm$wt_pooled)
  .tidy <- function(sf, g){
    lab <- gsub("^grp=", "", rep(names(sf$strata), sf$strata))
    d <- data.frame(
      time = sf$time,
      val = (1 - sf$surv) * 100,
      lower = (1 - sf$upper) * 100,
      upper = (1 - sf$lower) * 100,
      grp = lab
    )
    d[d$grp == g, , drop = FALSE]
  }
  sf_pre <- survfit(Surv(ftime_years, death_allcause) ~ grp, data = sub,
                    weights = w_km_pre, conf.type = "log-log")
  sf_post <- survfit(Surv(t_after, death_allcause) ~ grp, data = lm,
                     weights = w_km_landmark, conf.type = "log-log")
  .piece <- function(sf, g, end, phase, shift = 0){
    d <- .tidy(sf, g)
    d <- d[d$time <= end, , drop = FALSE]
    d <- rbind(data.frame(time = 0, val = 0, lower = 0, upper = 0, grp = g), d)
    last <- d[nrow(d), , drop = FALSE]
    if (last$time < end) {
      last$time <- end
      d <- rbind(d, last)
    }
    d$time <- d$time + shift
    d$phase <- phase
    d
  }
  cdf <- do.call(rbind, lapply(SHORT, function(g){
    rbind(.piece(sf_pre, g, LANDMARK, "Pre-landmark"),
          .piece(sf_post, g, XMAX - LANDMARK, "Post-landmark", LANDMARK))
  }))
  cdf$grp <- factor(cdf$grp, levels = SHORT)
  cdf$phase <- factor(cdf$phase, levels = c("Pre-landmark", "Post-landmark"))

  # Adjusted landmark Cox (Model 3 covariates; + bp_stage only for overall)
  rhs <- if (is_overall) paste("uacr_cat +", ADJ, "+ bp_stage") else paste("uacr_cat +", ADJ)
  hrlab <- "adjusted HR unavailable"
  fit <- tryCatch(svycoxph(as.formula(sprintf("Surv(t_after, death_allcause) ~ %s", rhs)),
                           design = des), error = function(e) NULL)
  if (is.null(fit))  # parsimonious fallback for sparse strata
    fit <- tryCatch(svycoxph(Surv(t_after, death_allcause) ~ uacr_cat + age_years + sex +
                             race_eth + dm_composite + egfr_2021, design = des),
                    error = function(e) NULL)
  hr12 <- hr30 <- c(NA, NA, NA)
  if (!is.null(fit)) {
    ci <- summary(fit)$conf.int; hr <- ci[grep("^uacr_cat", rownames(ci)), c(1,3,4), drop = FALSE]
    if (nrow(hr) >= 2) {
      hr12 <- hr[1, ]; hr30 <- hr[2, ]
      hrlab <- sprintf("10-29 vs <10: HR %.2f (%.2f-%.2f)\n>=30 vs <10: HR %.2f (%.2f-%.2f)",
                       hr12[1], hr12[2], hr12[3], hr30[1], hr30[2], hr30[3])
    }
  }
  nev <- sum(lm$death_allcause == 1)
  panel_letter <- if (is_overall) "Overall" else LETTERS[idx - 1]
  verif <<- rbind(verif, data.frame(
    Panel = panel_letter, Group = ttl_stub, N_landmark = nrow(lm), Events = nev,
    HR_10_29 = sprintf("%.2f (%.2f-%.2f)", hr12[1], hr12[2], hr12[3]),
    HR_ge30  = sprintf("%.2f (%.2f-%.2f)", hr30[1], hr30[2], hr30[3])))

  ttl <- sprintf("%s. %s\n(n=%s at landmark; %s deaths)", panel_letter,
                 if (is_overall) "Overall" else unname(BP_DISPLAY[ttl_stub]),
                 format(nrow(lm), big.mark = ","), format(nev, big.mark = ","))

  pc <- ggplot() +
    annotate("rect", xmin = 0, xmax = LANDMARK, ymin = -Inf, ymax = Inf,
             fill = "grey94", alpha = 0.75) +
    geom_vline(xintercept = LANDMARK, linetype = "dashed",
               color = "grey55", linewidth = 0.35) +
    geom_ribbon(
      data = cdf,
      aes(time, ymin = lower, ymax = upper, fill = grp,
          group = interaction(grp, phase)),
      alpha = 0.09, color = NA, show.legend = FALSE
    ) +
    geom_step(data = cdf,
              aes(time, val, color = grp, group = interaction(grp, phase)),
              linewidth = 0.85) +
    scale_color_manual(values = COLS, name = "UACR category") +
    scale_fill_manual(values = COLS, guide = "none") +
    scale_x_continuous(breaks = NAR_T, limits = c(0, XMAX),
                       expand = expansion(mult = c(0, 0), add = c(0, 0))) +
    scale_y_continuous(limits = c(0, YMAX), breaks = seq(0, YMAX, 10),
                       expand = expansion(mult = c(0, 0), add = c(0, 0))) +
    annotate("text", x = LANDMARK / 2, y = YMAX * 0.73,
             label = "Pre-landmark\nfollow-up", size = 2.0,
             color = "grey45", fontface = "italic") +
    annotate("label", x = LANDMARK + 0.22, y = YMAX * 0.93,
             hjust = 0, vjust = 1, size = 2.55,
             lineheight = 1.05, label = hrlab, fill = "white",
             label.r = unit(2, "pt")) +
    annotate("text", x = LANDMARK + 0.12, y = YMAX * 0.68,
             hjust = 0, vjust = 1, label = "2-year landmark\n(reset to 0)",
             size = 2.2, color = "grey45", fontface = "italic") +
    labs(title = ttl, x = NULL, y = "All-cause mortality (%)") +
    theme_classic(base_size = 10, base_family = "Helvetica") +
    theme(plot.title = element_text(face = "bold", size = 9.3, lineheight = 1.0),
          axis.title.y = element_text(size = 9),
          axis.text = element_text(size = 8.3, color = "black"),
          legend.position = "none", panel.grid = element_blank(),
          plot.margin = margin(4, 8, 0, 4))

  # number-at-risk strip
  nar <- expand.grid(t = NAR_T, grp = SHORT)
  nar$n <- mapply(function(t, g) sum(sub$grp == g & sub$ftime_years >= t), nar$t, nar$grp)
  nar$label_hjust <- ifelse(nar$t == 0, 0, 0.5)
  nar$grp <- factor(nar$grp, levels = rev(SHORT))
  pn <- ggplot(nar, aes(t, grp, label = format(n, big.mark = ","),
                        hjust = label_hjust)) +
    geom_text(size = 2.15, color = "black") +
    scale_x_continuous(breaks = NAR_T, limits = c(0, XMAX),
                       expand = expansion(mult = c(0, 0), add = c(0, 0))) +
    coord_cartesian(clip = "off") +
    scale_color_manual(values = COLS, guide = "none") +
    labs(
      x = "Years since baseline examination", y = NULL,
      subtitle = "No. at risk (unweighted participants)"
    ) +
    theme_classic(base_size = 10, base_family = "Helvetica") +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.y = element_text(size = 7.5, color = "black", hjust = 0),
          axis.text.x = element_text(size = 8.3, color = "black"),
          axis.title.x = element_text(size = 8.6),
          plot.subtitle = element_text(size = 8, face = "italic"),
          panel.grid = element_blank(), plot.margin = margin(0, 8, 4, 4))

  cowplot::plot_grid(pc, pn, ncol = 1, rel_heights = c(3.5, 1.15), align = "v", axis = "lr")
}

# common y-axis: sized to keep the highest-risk curve (Treated-Uncontrolled) un-clipped
YMAX <- 60
panels <- lapply(seq_along(PANELS), make_panel)

# shared legend
leg_src <- ggplot(coh, aes(uacr_log2, fill = grp)) + geom_bar() +
  scale_fill_manual(values = COLS, name = "UACR category") +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9.5))
legend <- cowplot::get_legend(leg_src)

# layout: A on top (full width), B-G in 3 rows x 2 cols
grid_bg <- cowplot::plot_grid(plotlist = panels[2:7], ncol = 2)
gtitle <- cowplot::ggdraw() + cowplot::draw_label(
  paste0(
    "Two-year landmark all-cause mortality by UACR within BP-treatment category\n",
    "Pre-landmark follow-up is lightly shaded; curves restart at 0 at year 2; bands show pointwise 95% CIs"
  ),
  fontface = "bold", size = 12, x = 0.5, hjust = 0.5)
final  <- cowplot::plot_grid(gtitle, grid_bg, legend, ncol = 1, rel_heights = c(0.065, 1, 0.05))

ggsave(file.path(OUTDIR, "Supplementary_Figure_Landmark2yr_BPxUACR.pdf"),
       final, width = 10.5, height = 12.0, device = grDevices::pdf,
       family = "Helvetica", bg = "white")
ggsave(file.path(OUTDIR, "Supplementary_Figure_Landmark2yr_BPxUACR.png"),
       final, width = 10.5, height = 12.0, dpi = 300, device = ragg::agg_png, bg = "white")
ggsave(file.path(OUTDIR, "Supplementary_Figure_Landmark2yr_BPxUACR.tiff"),
       final, width = 10.5, height = 12.0, dpi = 600, device = ragg::agg_tiff,
       compression = "lzw", bg = "white")

cat("\n================ 2-YR LANDMARK SUMMARY (all-cause) ================\n")
print(verif, row.names = FALSE)
write.csv(verif, file.path(OUTDIR, "Landmark2yr_BPxUACR_HR_table.csv"), row.names = FALSE)
cat("\n45_landmark2yr_bpxuacr.R complete.\n")
