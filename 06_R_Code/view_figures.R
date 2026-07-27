# Show saved figures in the RStudio Plots pane.
#
# The figure scripts write straight to disk, so nothing appears on screen while
# they run. These helpers read the PNG that was actually written and draw it, so
# what you see is what was saved, not a fresh render of the plot object.
#
#   source("06_R_Code/view_figures.R")
#   list_figures()            # what is available
#   show_figure("Figure_2")   # any part of the name is enough
#   show_figures()            # step through all of them, Enter to advance

.figdir <- "08_Analysis_Outputs/figures"

list_figures <- function(dir = .figdir) {
  files <- sort(list.files(dir, pattern = "\\.png$"))
  if (!length(files)) stop("No PNG files in ", dir)
  sub("\\.png$", "", files)
}

show_figure <- function(pattern, dir = .figdir) {
  files <- list.files(dir, pattern = "\\.png$", full.names = TRUE)
  hit <- grep(pattern, basename(files), ignore.case = TRUE, fixed = FALSE)
  if (!length(hit)) stop("No figure matching '", pattern, "'. Try list_figures().")
  if (length(hit) > 1) {
    message("Matched ", length(hit), ": ", paste(basename(files[hit]), collapse = ", "))
    message("Showing the first.")
  }
  path <- files[hit[1]]
  img <- png::readPNG(path)
  # Fit the image to the pane without distorting it.
  aspect <- dim(img)[1] / dim(img)[2]
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc") *
                                      min(1, 1 / aspect),
                                    height = grid::unit(1, "snpc") *
                                      min(1, aspect)))
  grid::grid.raster(img)
  grid::popViewport()
  message(basename(path), "  (", dim(img)[2], " x ", dim(img)[1], " px)")
  invisible(path)
}

show_figures <- function(dir = .figdir) {
  for (stub in list_figures(dir)) {
    show_figure(paste0("^", stub, "\\.png$"), dir)
    if (interactive()) readline("Enter for the next figure, Esc to stop: ")
  }
  invisible(NULL)
}
