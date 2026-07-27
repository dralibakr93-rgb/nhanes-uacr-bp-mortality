# =============================================================================
# helpers/04_theme_jacc.R — Publication-quality ggplot theme & palettes
# Aesthetic targets JACC / Circulation / European Heart Journal house style.
# =============================================================================

#' UACR 3-cat palette — colorblind-safe, JACC-aligned.
#' Normal (deep blue) → Low-grade (amber) → Albuminuria (deep red).
palette_uacr <- c(
  "Normal UACR (<10 mg/g)"               = "#1F4E79",
  "Low-grade albuminuria (10–29 mg/g)" = "#E08E2A",
  "Albuminuria (≥30 mg/g)"               = "#C0392B"
)
palette_uacr_short <- c(
  "Normal UACR"             = "#1F4E79",
  "Low-grade albuminuria" = "#E08E2A",
  "Albuminuria"             = "#C0392B"
)
palette_uacr_legend <- c(
  "Normal"      = "#1F4E79",
  "Low-grade" = "#E08E2A",
  "Albuminuria" = "#C0392B"
)

#' Sex / mortality / BP-stage palettes (extend as needed).
palette_bp <- c(
  "Non-elevated BP"        = "#2C7BB6",
  "Elevated BP"            = "#ABD9E9",
  "Stage 1 hypertension"   = "#FDAE61",
  "Stage 2 hypertension"   = "#D7191C",
  "Treated - Controlled"   = "#1A9850",
  "Treated - Uncontrolled" = "#7B3294"
)

#' Publication-quality theme.
theme_jacc <- function(base_size = 11, base_family = "") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title           = ggplot2::element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.subtitle        = ggplot2::element_text(size = base_size - 1, color = "grey30"),
      axis.title           = ggplot2::element_text(face = "bold", size = base_size),
      axis.text            = ggplot2::element_text(size = base_size - 1, color = "black"),
      axis.line            = ggplot2::element_line(colour = "black", linewidth = 0.4),
      axis.ticks           = ggplot2::element_line(colour = "black", linewidth = 0.4),
      legend.position      = "bottom",
      legend.title         = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.text          = ggplot2::element_text(size = base_size - 1),
      legend.background    = ggplot2::element_blank(),
      legend.key           = ggplot2::element_blank(),
      strip.background     = ggplot2::element_rect(fill = "grey92", colour = NA),
      strip.text           = ggplot2::element_text(face = "bold", size = base_size),
      panel.grid           = ggplot2::element_blank(),
      plot.background      = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background     = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin          = ggplot2::margin(8, 12, 6, 10)
    )
}

#' High-resolution multi-format figure export (TIFF/PDF/PNG @600 dpi).
#' Cairo-graceful: probes cairo at runtime (capabilities() is unreliable on
#' macOS where R is built with cairo but X11 libs are absent) and falls back
#' to base `pdf()` if cairo doesn't actually work. Each format is attempted
#' independently inside tryCatch so one failure doesn't abort the others.
save_fig_all <- function(plot, basename,
                         width = 7, height = 5, dpi = 600,
                         dir = NULL, bg = "white") {
  if (is.null(dir)) dir <- get0("PATHS", ifnotfound = list(figures = "results/figures"))$figures
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # Cached cairo runtime probe — actually try to OPEN a cairo PDF.
  # Many macOS R installs are built with cairo but lack the X11 libs needed
  # at runtime, so capabilities("cairo") returns TRUE while cairo_pdf() warns
  # ("failed to load cairo DLL") and silently writes a broken file.
  #
  # Earlier versions of this probe were wrong because the post-call assign()
  # always overwrote whatever the warning handler had set. We now use a
  # LOCAL flag captured by `<<-` so a warning observed during cairo_pdf()
  # definitively flips cairo_ok to FALSE regardless of whether the tryCatch
  # ultimately returned TRUE.
  #
  # The cache key is *versioned* (`_v2`) so stale TRUE values written by
  # earlier (buggy) probe versions are ignored — without this, a long-running
  # R session that source()'d the old probe will continue to think cairo
  # works even after the file is patched.
  cairo_ok <- get0(".save_fig_cairo_ok_v2", envir = .GlobalEnv,
                   ifnotfound = NULL)
  if (is.null(cairo_ok)) {
    cairo_failed <- FALSE
    probe_result <- tryCatch(
      withCallingHandlers({
        tmp <- tempfile(fileext = ".pdf")
        grDevices::cairo_pdf(tmp, width = 1, height = 1)
        graphics::plot.new()
        grDevices::dev.off()
        unlink(tmp)
        TRUE
      },
      warning = function(w) {
        if (grepl("cairo|DLL|libSM|libXrender|X11",
                  conditionMessage(w), ignore.case = TRUE)) {
          cairo_failed <<- TRUE
          invokeRestart("muffleWarning")
        }
      }),
      error = function(e) { cairo_failed <<- TRUE; FALSE }
    )
    cairo_ok <- isTRUE(probe_result) && !cairo_failed
    assign(".save_fig_cairo_ok_v2", cairo_ok, envir = .GlobalEnv)
    # Clean up any stale older-version cache so subsequent helpers that might
    # check the legacy key don't see the wrong value.
    if (exists(".save_fig_cairo_ok", envir = .GlobalEnv, inherits = FALSE))
      rm(".save_fig_cairo_ok", envir = .GlobalEnv)
    if (!isTRUE(cairo_ok)) {
      message("save_fig_all: cairo unavailable at runtime — using base pdf() for all PDF exports.")
    }
  }

  # PDF: cairo if available, base pdf otherwise. Belt-and-suspenders fallback —
  # if cairo_ok is TRUE but ggsave nonetheless emits the "failed to load cairo
  # DLL" warning, we catch it and retry with base pdf(). This guards against
  # any case where cairo "works" enough to pass the probe but fails on a
  # particular call (e.g. font availability differences).
  .save_pdf <- function() {
    cairo_warned <- FALSE
    pdf_dev <- if (isTRUE(cairo_ok)) grDevices::cairo_pdf else grDevices::pdf
    tryCatch(
      withCallingHandlers(
        ggplot2::ggsave(file.path(dir, paste0(basename, ".pdf")),
                        plot, width = width, height = height, units = "in",
                        device = pdf_dev, bg = bg),
        warning = function(w) {
          if (grepl("cairo|DLL|libSM|libXrender|X11",
                    conditionMessage(w), ignore.case = TRUE)) {
            cairo_warned <<- TRUE
            invokeRestart("muffleWarning")
          }
        }),
      error = function(e) {
        warning("PDF export failed for ", basename, ": ",
                conditionMessage(e), call. = FALSE)
      })
    if (isTRUE(cairo_warned)) {
      # Cairo silently failed; invalidate the cache and retry with base pdf().
      assign(".save_fig_cairo_ok_v2", FALSE, envir = .GlobalEnv)
      tryCatch(
        ggplot2::ggsave(file.path(dir, paste0(basename, ".pdf")),
                        plot, width = width, height = height, units = "in",
                        device = grDevices::pdf, bg = bg),
        error = function(e) {
          warning("PDF retry with base pdf() failed for ", basename, ": ",
                  conditionMessage(e), call. = FALSE)
        })
    }
  }
  .save_pdf()

  # PNG: prefer ragg::agg_png (clean fonts); fall back to default png device
  tryCatch({
    if (requireNamespace("ragg", quietly = TRUE)) {
      ggplot2::ggsave(file.path(dir, paste0(basename, ".png")),
                      plot, width = width, height = height, dpi = dpi,
                      units = "in", device = ragg::agg_png, bg = bg)
    } else {
      ggplot2::ggsave(file.path(dir, paste0(basename, ".png")),
                      plot, width = width, height = height, dpi = dpi,
                      units = "in", bg = bg)
    }
  }, error = function(e) {
    warning("PNG export failed for ", basename, ": ", conditionMessage(e),
            call. = FALSE)
  })

  # TIFF: ragg if available, else built-in
  tryCatch({
    if (requireNamespace("ragg", quietly = TRUE)) {
      ggplot2::ggsave(file.path(dir, paste0(basename, ".tiff")),
                      plot, width = width, height = height, dpi = dpi,
                      units = "in", device = ragg::agg_tiff,
                      compression = "lzw", bg = bg)
    } else {
      ggplot2::ggsave(file.path(dir, paste0(basename, ".tiff")),
                      plot, width = width, height = height, dpi = dpi,
                      units = "in", device = "tiff", compression = "lzw",
                      bg = bg)
    }
  }, error = function(e) {
    warning("TIFF export failed for ", basename, ": ", conditionMessage(e),
            call. = FALSE)
  })

  invisible(basename)
}
