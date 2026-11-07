# What every easyPR figure shares.
#
# Same contract as easyDR: a plot takes the fitted object, draws it, and
# writes it to a file only if the caller named one. `result = FALSE` keeps
# the console quiet when the figure is the only thing wanted, which is what
# a chunk in the book usually wants.

#' @keywords internal
#' @noRd
pr_theme <- function(base = 12) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey35"),
      legend.position = "bottom"
    )
}

#' Where a figure is written, if anywhere
#' @keywords internal
#' @noRd
pr_file_spec <- function(file.name, path, filetype, width, height, res = 300) {
  if (is.null(file.name)) return(NULL)
  filetype <- match.arg(filetype, c("png", "pdf", "tiff"))
  if (is.null(path)) path <- "."
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  ext <- paste0(".", filetype)
  if (!grepl(paste0("\\", ext, "$"), file.name, ignore.case = TRUE)) {
    file.name <- paste0(file.name, ext)
  }
  list(file = file.path(path, file.name), filetype = filetype,
       width = width, height = height, res = res)
}

#' Write the plot and say where it went
#' @keywords internal
#' @noRd
pr_write <- function(p, spec, width, height) {
  if (is.null(spec)) return(invisible(NULL))
  w <- if (is.null(spec$width))  width  else spec$width
  h <- if (is.null(spec$height)) height else spec$height
  ggplot2::ggsave(spec$file, p, width = w, height = h, dpi = spec$res,
                  bg = "white")
  message("easypr: wrote ", spec$file, "  (", format(round(w, 2)), " x ",
          format(round(h, 2)), " inches)")
  invisible(spec$file)
}

#' Refuse clearly when the object is not a fit
#' @keywords internal
#' @noRd
pr_check <- function(x, what) {
  if (!inherits(x, "easypr_result")) {
    pr_stop(what, " takes a fitted result from easypr().")
  }
  invisible(TRUE)
}

#' Make study labels unique for plotting
#'
#' Two validations by the same author in the same review is ordinary, and
#' EuroSCORE has two by Wang and two by Barilli. A duplicated label cannot be
#' a factor level, so the second and later ones are suffixed. The table keeps
#' the original labels; only the axis is changed.
#'
#' @keywords internal
#' @noRd
pr_unique_labels <- function(x) {
  x <- as.character(x)
  dup <- duplicated(x) | duplicated(x, fromLast = TRUE)
  if (!any(dup)) return(x)
  for (nm in unique(x[dup])) {
    i <- which(x == nm)
    x[i] <- paste0(nm, " (", seq_along(i), ")")
  }
  x
}


#' Back-transform for display, and the matching axis
#'
#' The ratio measure is pooled on the log scale, which is where a symmetric
#' interval belongs, but nobody reads a log hazard ratio off a plot. Every
#' figure therefore exponentiates the values it draws and puts the axis on a
#' log scale, so the spacing stays the pooling's and the labels stay the
#' reader's.
#'
#' @keywords internal
#' @noRd
pr_back <- function(x) if (isTRUE(x$log)) exp else identity

#' @keywords internal
#' @noRd
pr_scale_x <- function(x) {
  if (isTRUE(x$log)) {
    ggplot2::scale_x_continuous(trans = "log",
                                breaks = scales::log_breaks(n = 6, base = exp(1)),
                                labels = function(v) formatC(v, format = "g", digits = 2))
  } else {
    ggplot2::scale_x_continuous()
  }
}

#' Where the null sits, if there is one
#' @keywords internal
#' @noRd
pr_null_at <- function(x) {
  if (isTRUE(x$log)) return(1)
  if (identical(x$measure, "OE")) return(1)
  NA_real_
}
