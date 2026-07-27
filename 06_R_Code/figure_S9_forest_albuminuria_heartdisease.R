# Supplementary Figure S9: albuminuria subgroup forest (heart disease).
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")
suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
cohort$age_group_60 <- factor(
  ifelse(cohort$age < 60, "<60 years", ">=60 years"),
  levels = c("<60 years", ">=60 years")
)
cohort$sex_group <- factor(
  ifelse(cohort$sex_female == 1, "Women", "Men"),
  levels = c("Men", "Women")
)
cohort$bmi_group <- cut(
  cohort$bmi, c(-Inf, 18.5, 25, 30, Inf), right = FALSE,
  labels = c("Underweight", "Normal weight", "Overweight", "Obesity")
)
cohort$glycemic_group <- factor(
  ifelse(cohort$dm_main == 1, "Diabetes",
         ifelse(cohort$hba1c >= 5.7, "Prediabetes", "Normoglycemia")),
  levels = c("Normoglycemia", "Prediabetes", "Diabetes")
)
cohort$dyslipidemia_group <- factor(
  ifelse(cohort$dyslipidemia_main == 1, "Yes", "No"),
  levels = c("No", "Yes")
)
cohort$egfr_group <- cut(
  cohort$egfr, c(60, 75, 90, Inf), right = FALSE,
  labels = c("60-74", "75-89", ">=90")
)
cohort$egfr_group <- factor(cohort$egfr_group, levels = c(">=90", "75-89", "60-74"))
cohort$survey_period <- factor(
  ifelse(cohort$cycle <= 5, "1999-2008", "2009-2018"),
  levels = c("1999-2008", "2009-2018")
)
full_design <- make_design(cohort)

bp_labels <- c(
  "Non-elevated BP" = "Non-elevated BP",
  "Elevated BP" = "Elevated BP",
  "Stage 1 hypertension" = "Untreated stage 1 HTN",
  "Stage 2 hypertension" = "Untreated stage 2 HTN",
  "Treated - Controlled" = "Treated-controlled HTN",
  "Treated - Uncontrolled" = "Treated-uncontrolled HTN"
)
race_labels <- c(
  "Non-Hispanic White" = "NH White",
  "Non-Hispanic Black" = "NH Black",
  "Mexican American" = "Mexican American",
  "Other Hispanic" = "Other Hispanic",
  "Other / Multiracial" = "Other/multiracial"
)

format_events <- function(events, n) {
  sprintf("%s/%s", format(events, big.mark = ","), format(n, big.mark = ","))
}

format_pint <- function(value) {
  out <- rep("", length(value))
  keep <- !is.na(value)
  out[keep] <- ifelse(value[keep] < 0.001, "<0.001", sprintf("%.3f", value[keep]))
  sub("0+$", "", sub("\\.$", "", out))
}

display_level <- function(variable, level) {
  if (variable == "bp_stage") return(unname(bp_labels[level]))
  if (variable == "race_eth" && level %in% names(race_labels)) {
    return(unname(race_labels[level]))
  }
  level
}

specifications <- list(
  "Age, y" = list(section = "Demographics", variable = "age_group_60", omit = "age", omit_est = character()),
  "Sex" = list(section = "Demographics", variable = "sex_group", omit = "sex_female"),
  "Race or ethnicity" = list(section = "Demographics", variable = "race_eth", omit = "race_eth"),
  "Body-mass index" = list(section = "Cardiometabolic factors", variable = "bmi_group", omit = "bmi", omit_est = character()),
  "Glycemic status" = list(section = "Cardiometabolic factors", variable = "glycemic_group", omit = "dm_main"),
  "Smoking status" = list(section = "Cardiometabolic factors", variable = "smoking", omit = "smoking"),
  "Dyslipidemia" = list(section = "Cardiometabolic factors", variable = "dyslipidemia_group", omit = character()),
  "Kidney function" = list(section = "Kidney function", variable = "egfr_group", omit = "egfr", omit_est = character()),
  "Survey period" = list(section = "Survey period", variable = "survey_period", omit = "cycle")
)

fit_exposure <- function(fit_design, outcome, adjustment, exposure_level) {
  formula <- as.formula(paste0(
    "Surv(ftime_years, ", outcome, ") ~ uacr_cat + ",
    paste(adjustment, collapse = " + ")
  ))
  fit <- quietly(svycoxph(formula, design = fit_design))
  summary_fit <- quietly(summary(fit))
  row_index <- grep(paste0("uacr_cat", exposure_level),
                    rownames(summary_fit$conf.int), fixed = TRUE)[1]
  c(
    HR = summary_fit$conf.int[row_index, 1],
    lower = summary_fit$conf.int[row_index, 3],
    upper = summary_fit$conf.int[row_index, 4]
  )
}

interaction_p <- function(outcome, variable, adjustment, exposure_level) {
  ref <- levels(cohort$uacr_cat)[1]
  design2 <- subset(full_design, uacr_cat %in% c(ref, exposure_level))
  design2 <- update(design2, uacr_c2 = droplevels(factor(uacr_cat)))
  formula <- as.formula(paste0(
    "Surv(ftime_years, ", outcome, ") ~ uacr_c2 * ", variable,
    " + ", paste(adjustment, collapse = " + ")
  ))
  fit <- quietly(svycoxph(formula, design = design2))
  quietly(regTermTest(fit, as.formula(paste0("~uacr_c2:", variable))))$p
}

count_events <- function(outcome, exposure_level, adjustment,
                         variable = NULL, level = NULL) {
  selected <- complete.cases(cohort[, unique(c(
    "ftime_years", outcome, "uacr_cat", adjustment
  ))])
  if (!is.null(variable)) selected <- selected & cohort[[variable]] == level
  normal <- selected & cohort$uacr_cat == levels(cohort$uacr_cat)[1]
  exposed <- selected & cohort$uacr_cat == exposure_level
  c(
    normal_events = sum(cohort[[outcome]][normal], na.rm = TRUE),
    normal_n = sum(normal, na.rm = TRUE),
    exposed_events = sum(cohort[[outcome]][exposed], na.rm = TRUE),
    exposed_n = sum(exposed, na.rm = TRUE)
  )
}

row_record <- function(row_type, label, HR = NA_real_, lower = NA_real_,
                       upper = NA_real_, interaction = NA_real_,
                       normal_events = NA_integer_, normal_n = NA_integer_,
                       exposed_events = NA_integer_, exposed_n = NA_integer_) {
  data.frame(
    row_type = row_type, label = label, HR = HR, lower = lower, upper = upper,
    interaction_p = interaction,
    normal_events = normal_events, normal_n = normal_n,
    exposed_events = exposed_events, exposed_n = exposed_n,
    stringsAsFactors = FALSE
  )
}

build_forest_data <- function(outcome, exposure_level) {
  result <- list()
  active_section <- NULL
  for (group_name in names(specifications)) {
    specification <- specifications[[group_name]]
    adjustment <- setdiff(model_terms$M3, specification$omit)
    omit_est <- if (is.null(specification$omit_est)) specification$omit else specification$omit_est
    adjustment_est <- setdiff(model_terms$M3, omit_est)
    p_value <- tryCatch(
      interaction_p(outcome, specification$variable, adjustment, exposure_level),
      error = function(error) NA_real_
    )
    section_is_variable <- identical(group_name, specification$section)
    if (!identical(active_section, specification$section)) {
      active_section <- specification$section
      result[[length(result) + 1]] <- row_record(
        "section", active_section,
        interaction = if (section_is_variable) p_value else NA_real_
      )
    }
    if (!section_is_variable) {
      result[[length(result) + 1]] <- row_record(
        "variable", group_name, interaction = p_value
      )
    }
    variable_values <- cohort[[specification$variable]]
    for (level in levels(variable_values)) {
      selected <- which(variable_values == level)
      estimate <- tryCatch(
        {
          subgroup_design <- full_design[selected, ]
          fit_exposure(subgroup_design, outcome, adjustment_est, exposure_level)
        },
        error = function(error) c(HR = NA_real_, lower = NA_real_, upper = NA_real_)
      )
      counts <- count_events(
        outcome, exposure_level, adjustment_est, specification$variable, level
      )
      result[[length(result) + 1]] <- row_record(
        "level", paste0("   ", display_level(specification$variable, level)),
        HR = estimate["HR"], lower = estimate["lower"], upper = estimate["upper"],
        normal_events = counts["normal_events"], normal_n = counts["normal_n"],
        exposed_events = counts["exposed_events"], exposed_n = counts["exposed_n"]
      )
    }
  }
  overall <- fit_exposure(full_design, outcome, model_terms$M3, exposure_level)
  counts <- count_events(outcome, exposure_level, model_terms$M3)
  result[[length(result) + 1]] <- row_record(
    "overall", "Overall (complete-case Model 3)",
    HR = overall["HR"], lower = overall["lower"], upper = overall["upper"],
    normal_events = counts["normal_events"], normal_n = counts["normal_n"],
    exposed_events = counts["exposed_events"], exposed_n = counts["exposed_n"]
  )
  data <- do.call(rbind, result)
  data$y <- rev(seq_len(nrow(data)))
  data$estimate_text <- ifelse(
    is.na(data$HR), "",
    sprintf("%.2f (%.2f-%.2f)", data$HR, data$lower, data$upper)
  )
  data$normal_text <- ifelse(
    is.na(data$normal_n), "",
    format_events(data$normal_events, data$normal_n)
  )
  data$exposed_text <- ifelse(
    is.na(data$exposed_n), "",
    format_events(data$exposed_events, data$exposed_n)
  )
  data$interaction_text <- ifelse(
    data$row_type %in% c("variable", "section"),
    format_pint(data$interaction_p), ""
  )
  data$lower_plot <- pmax(data$lower, 0.22, na.rm = TRUE)
  data$upper_plot <- pmin(data$upper, 4.8, na.rm = TRUE)
  data
}

plot_forest_table <- function(data, title_text, subtitle_text, exposure_header,
                              accent = "#1F4E79") {
  plot_rows <- subset(data, !is.na(HR))
  section_rows <- subset(data, row_type == "section")
  overall_row <- subset(data, row_type == "overall")
  y_top <- max(data$y) + 1.0
  x_min <- 0.12
  x_max <- 24
  ggplot(data, aes(HR, y)) +
    geom_rect(data = section_rows,
              aes(xmin = x_min, xmax = x_max, ymin = y - 0.43, ymax = y + 0.43),
              inherit.aes = FALSE, fill = "grey91", color = NA) +
    geom_hline(yintercept = y_top - 0.55, color = "grey35", linewidth = 0.55) +
    geom_hline(yintercept = overall_row$y + 0.55, color = "grey35", linewidth = 0.55) +
    geom_vline(xintercept = 1, color = "grey62", linewidth = 0.55) +
    geom_vline(xintercept = overall_row$HR, linetype = "dashed",
               color = accent, linewidth = 0.65) +
    geom_segment(data = plot_rows,
                 aes(x = lower_plot, xend = upper_plot, y = y, yend = y),
                 linewidth = 0.62, color = "grey30") +
    geom_point(data = plot_rows, aes(x = HR, y = y),
               shape = 15, size = 2.2, color = accent) +
    geom_text(aes(x = x_min, label = label,
                  fontface = ifelse(row_type %in% c("section", "variable", "overall"),
                                    "bold", "plain")),
              hjust = 0, size = 3.0, color = "grey16") +
    geom_text(aes(x = 6.8, label = normal_text), hjust = 0.5,
              size = 2.75, color = "grey32") +
    geom_text(aes(x = 9.3, label = exposed_text), hjust = 0.5,
              size = 2.75, color = "grey32") +
    geom_text(aes(x = 12.1, label = estimate_text,
                  fontface = ifelse(row_type == "overall", "bold", "plain")),
              hjust = 0, size = 2.85, color = "grey16") +
    geom_text(aes(x = 20.0, label = interaction_text,
                  fontface = ifelse(row_type %in% c("variable", "section"),
                                    "bold", "plain")),
              hjust = 0.5, size = 2.95, color = "grey16") +
    annotate("text", x = x_min, y = y_top, label = "Subgroup",
             hjust = 0, fontface = "bold", size = 3.2, color = "grey12") +
    annotate("text", x = 6.8, y = y_top,
             label = "Normal\nevents/N", hjust = 0.5,
             fontface = "bold", size = 2.8, lineheight = 0.9) +
    annotate("text", x = 9.3, y = y_top,
             label = paste0(exposure_header, "\nevents/N"), hjust = 0.5,
             fontface = "bold", size = 2.8, lineheight = 0.9) +
    annotate("text", x = 12.1, y = y_top, label = "HR (95% CI)",
             hjust = 0, fontface = "bold", size = 3.0) +
    annotate("text", x = 20.0, y = y_top, label = "P interaction",
             hjust = 0.5, fontface = "bold", size = 3.0) +
    annotate("text", x = 0.30, y = 0.35, label = "Lower risk",
             hjust = 0, size = 2.55, color = "grey50") +
    annotate("text", x = 1.05, y = 0.35,
             label = paste0("Higher risk with ", tolower(exposure_header)),
             hjust = 0, size = 2.55, color = "grey50") +
    scale_x_log10(
      limits = c(x_min, x_max),
      breaks = c(0.25, 0.50, 1.00, 2.00, 4.00),
      labels = c("0.25", "0.50", "1.00", "2.00", "4.00")
    ) +
    scale_y_continuous(limits = c(0, y_top + 0.65), expand = c(0, 0)) +
    labs(title = title_text, subtitle = subtitle_text,
         x = "Adjusted hazard ratio (95% CI, log scale)", y = NULL) +
    theme_minimal(base_size = 10.5, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 13.2, color = "grey7"),
      plot.subtitle = element_text(size = 9.5, color = "grey32",
                                   margin = margin(b = 12)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 8.6, color = "grey25"),
      axis.title.x = element_text(face = "bold", size = 9.0),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(8, 12, 8, 12)
    )
}

save_plot <- function(plot, filename, width = 13.8, height = 12.3) {
  ggsave(file.path(paths$figures, paste0(filename, ".pdf")), plot,
         width = width, height = height, device = grDevices::pdf)
  ggsave(file.path(paths$figures, paste0(filename, ".png")), plot,
         width = width, height = height, dpi = 300,
         device = ragg::agg_png, bg = "white")
  ggsave(file.path(paths$figures, paste0(filename, ".tiff")), plot,
         width = width, height = height, dpi = 600,
         device = ragg::agg_tiff, compression = "lzw", bg = "white")
}
albuminuria_heart <- build_forest_data(
  "death_hd", "Albuminuria (>=30 mg/g)"
)
save_plot(
  plot_forest_table(
    albuminuria_heart,
    "Albuminuria (>=30 mg/g) and heart-disease mortality across subgroups",
    "Adjusted HRs compare UACR >=30 mg/g with UACR <10 mg/g. Secondary/exploratory outcome; fewer events yield wider CIs.",
    "Albumin.", "#7E241B"
  ),
  "Supplementary_Figure_S9_Forest_Albuminuria_HeartDisease"
)
