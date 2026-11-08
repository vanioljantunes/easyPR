# The pieces a forest plot is built from.
#
# The forest is drawn the way easyDR draws its own, which is the way {meta}
# draws one: three panels side by side, the labels on the left, the effect in
# the middle and the numbers on the right, every row the same height. The code
# below is easyDR's, renamed, because a reader who has seen the figure in one
# of these packages should recognise it in the other.

#' A p value, and one that carries its own operator
#' @keywords internal
#' @noRd
pr_p <- function(p, digits = 3) {
  if (length(p) != 1L || !is.finite(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  format(round(p, digits), nsmall = digits)
}

#' @keywords internal
#' @noRd
pr_p_lab <- function(p, digits = 3) {
  v <- pr_p(p, digits)
  if (startsWith(v, "<")) paste("p", v) else paste("p =", v)
}

#' The theme every panel of the forest shares
#'
#' Panels are composed side by side, so they must reserve identical bottom
#' space for the axis ink or the study rows will not line up. A panel with no
#' axis to show draws it in `NA` colour rather than dropping it.
#'
#' @keywords internal
#' @noRd
pr_gg_theme <- function(axis = FALSE, axis_text_size = 8) {
  base <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y    = ggplot2::element_blank(),
      axis.text.y     = ggplot2::element_blank(),
      axis.ticks.y    = ggplot2::element_blank(),
      panel.grid      = ggplot2::element_blank(),
      panel.border    = ggplot2::element_blank(),
      plot.margin     = ggplot2::margin(t = 5.5, r = 0, b = 2, l = 0),
      legend.position = "none"
    )
  # The axis title reserves its height on every panel, visible only on the one
  # that has an axis. Dropping it on the others makes their panel areas taller
  # and the study rows stop lining up across the figure.
  ink <- if (isTRUE(axis)) "black" else NA
  base + ggplot2::theme(
    axis.text.x  = ggplot2::element_text(colour = ink, size = axis_text_size),
    axis.ticks.x = ggplot2::element_line(colour = ink, linewidth = 0.4),
    axis.line.x  = ggplot2::element_line(colour = ink, linewidth = 0.4),
    axis.title.x = ggplot2::element_text(colour = ink, size = 9,
                                         margin = ggplot2::margin(t = 4)),
    axis.ticks.length = grid::unit(3, "pt"))
}

#' Stripe every other study row, and never a summary row
#' @keywords internal
#' @noRd
pr_zebra <- function(ypos) {
  df <- data.frame(ypos = ypos[seq_along(ypos) %% 2L == 1L])
  ggplot2::geom_rect(
    data = df, inherit.aes = FALSE,
    ggplot2::aes(xmin = -Inf, xmax = Inf,
                 ymin = .data$ypos - 0.5, ymax = .data$ypos + 0.5),
    fill = "grey92")
}

#' A panel of text columns, headed in bold
#'
#' Column positions come from character widths, so a long study label cannot
#' push a numeric column off the panel. Numeric columns are right-aligned
#' against their own edge and text columns left-aligned against their start.
#'
#' @keywords internal
#' @noRd
pr_text_panel <- function(cols, right, ypos, face, header_y, ylim, zebra,
                          notes = NULL, cell = 3.0, hdr = 3.2, xlab = " ") {
  w <- vapply(seq_along(cols), function(i)
    max(nchar(c(names(cols)[i], as.character(cols[[i]]))), na.rm = TRUE) +
      if (right[i]) 1.5 else 2.5, numeric(1))
  edge  <- cumsum(w) / sum(w)
  start <- c(0, utils::head(edge, -1))
  xs    <- ifelse(right, edge, start)

  cells <- do.call(rbind, lapply(seq_along(cols), function(i) data.frame(
    x = xs[i], ypos = ypos, label = as.character(cols[[i]]), face = face,
    hjust = if (right[i]) 1 else 0, stringsAsFactors = FALSE)))
  cells <- cells[!is.na(cells$label) & cells$label != "", , drop = FALSE]
  heads <- data.frame(x = xs, ypos = header_y, label = names(cols),
                      hjust = ifelse(right, 1, 0), stringsAsFactors = FALSE)

  p <- ggplot2::ggplot(cells, ggplot2::aes(y = .data$ypos)) +
    zebra +
    ggplot2::geom_text(ggplot2::aes(x = .data$x, label = .data$label,
                                    fontface = .data$face,
                                    hjust = .data$hjust), size = cell) +
    ggplot2::geom_text(data = heads,
                       ggplot2::aes(x = .data$x, label = .data$label,
                                    hjust = .data$hjust),
                       fontface = "bold", size = hdr)
  for (n in notes) {
    p <- p + ggplot2::annotate("text", x = 0, y = n$y, label = n$label,
                               hjust = 0, size = cell)
  }
  p +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = ylim) +
    ggplot2::labs(x = xlab) +
    pr_gg_theme(axis = FALSE)
}

#' Width of a text panel in inches, from the characters it has to hold
#' @keywords internal
#' @noRd
pr_panel_width <- function(cols, per_char = 0.070, min_in = 1.2,
                           notes = NULL) {
  chars <- sum(vapply(seq_along(cols), function(i)
    max(nchar(c(names(cols)[i], as.character(cols[[i]]))), na.rm = TRUE) + 2,
    numeric(1)))
  # A note is written across the whole panel, so a long one sets the width on
  # its own rather than running off the edge.
  if (length(notes) > 0L) {
    chars <- max(chars, max(nchar(vapply(notes, function(n) n$label,
                                         character(1)))) + 2)
  }
  max(min_in, per_char * chars)
}

#' Stack a title, a body and an optional footer, and draw them
#'
#' A forest has a fixed height per row, so its composite is centred in the
#' device at exactly the height it needs rather than stretched to fill it.
#'
#' @keywords internal
#' @noRd
pr_draw_stack <- function(body, body_h = NULL, title = NULL, footer = NULL,
                          footer_h = NULL, title_size = 12, footer_size = 9,
                          body_w = NULL, title_hjust = 0) {
  fill    <- is.null(body_h)
  grobs   <- list(body)
  heights <- list(if (fill) grid::unit(1, "null") else body_h)
  if (!is.null(title)) {
    tx <- if (title_hjust == 0) grid::unit(4, "pt") else
      grid::unit(title_hjust, "npc")
    grobs   <- c(list(grid::textGrob(
      title, x = tx, hjust = title_hjust,
      gp = grid::gpar(fontface = "bold", fontsize = title_size))), grobs)
    heights <- c(list(grid::unit(1.9 * title_size, "points")), heights)
  }
  if (!is.null(footer)) {
    if (is.character(footer)) {
      n <- length(strsplit(paste(footer, collapse = "\n"), "\n",
                           fixed = TRUE)[[1]])
      footer <- grid::textGrob(
        paste(footer, collapse = "\n"), x = grid::unit(4, "pt"),
        y = grid::unit(1, "npc"), hjust = 0, vjust = 1,
        gp = grid::gpar(fontsize = footer_size, lineheight = 1.25))
      if (is.null(footer_h)) {
        footer_h <- grid::unit(1.35 * footer_size * n + 8, "points")
      }
    }
    if (is.null(footer_h)) footer_h <- grid::unit(1, "null")
    grobs   <- c(grobs, list(footer))
    heights <- c(heights, list(footer_h))
  }
  hv <- do.call(grid::unit.c, heights)
  g  <- gridExtra::arrangeGrob(grobs = grobs, ncol = 1, heights = hv)
  grid::grid.newpage()
  # A composite of fixed-width panels is centred at that width rather than
  # stretched across the device, or the value column drifts away from the
  # interval it belongs to on a wide device.
  w <- if (is.null(body_w)) grid::unit(1, "npc") else body_w
  vp <- if (fill) {
    grid::viewport(width = w, height = grid::unit(1, "npc"))
  } else {
    grid::viewport(x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
                   just = "centre", width = w, height = Reduce(`+`, heights))
  }
  grid::pushViewport(vp)
  grid::grid.draw(g)
  grid::popViewport()
  invisible(g)
}

#' Axis ticks on the reader's scale, for an axis drawn on the pooling scale
#'
#' The panel is drawn on the scale the pooling happened on, because that is
#' where an interval is symmetric. The labels are the numbers the reader
#' reported, so a c-statistic axis reads 0.6, 0.7, 0.8 even though the
#' spacing along it is the logit's.
#'
#' @keywords internal
#' @noRd
pr_ticks <- function(lo, hi, scale) {
  if (identical(scale, "identity")) {
    v <- pretty(c(lo, hi))
    return(list(at = v, labels = format(v)))
  }
  f <- pr_fwd(scale)
  b <- pr_bwd(scale)
  if (identical(scale, "log")) {
    # pretty() spaces its values evenly, and evenly spaced values on a log
    # axis pile up at the right: a forest running from 0.1 to 100 came back
    # ticked 20, 40, 60, 80, 100 with nothing under 1 at all. A ratio axis
    # wants a geometric ladder, which is what a reader of one expects.
    # Doubling, so that thinning a crowded axis leaves the rungs evenly
    # spaced. A ladder mixing 5s and 10s with the powers of two thinned to
    # 0.1, 0.25, 1, 4, 8, 16, 25, 100, which reads as an accident.
    ladder <- 2^(-7:11)
    v <- ladder[ladder >= b(lo) & ladder <= b(hi)]
    # Thinning keeps the no-effect rung, because an axis that has dropped 1
    # is an axis a reader cannot place the diamond against.
    while (length(v) > 8L) {
      anchor <- which.min(abs(v - 1))
      v <- v[seq(((anchor - 1L) %% 2L) + 1L, length(v), by = 2L)]
    }
    if (length(v) < 2L) v <- pretty(c(b(lo), b(hi)))[-1L]
    return(list(at = f(v), labels = format(v, drop0trailing = TRUE)))
  }
  v <- pretty(c(b(lo), b(hi)))
  v <- v[v > 0 & v < 1]
  list(at = f(v), labels = format(v))
}

#' Measure without leaving an Rplots.pdf behind
#'
#' Converting a grid unit to inches needs an open device. A session started
#' with `Rscript` has none, and R would open `Rplots.pdf` in the working
#' directory as a side effect of the measurement.
#'
#' @keywords internal
#' @noRd
pr_measure <- function(expr) {
  if (is.null(grDevices::dev.list())) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  expr
}

#' Height of a grid unit in inches, or zero when it cannot be measured
#' @keywords internal
#' @noRd
pr_height_in <- function(u) {
  if (is.null(u)) return(0)
  v <- try(pr_measure(grid::convertHeight(u, "in", valueOnly = TRUE)),
           silent = TRUE)
  if (inherits(v, "try-error") || !is.finite(v)) 0 else sum(v)
}

#' Hold a figure size inside the range a page can carry
#'
#' The measured size is what the figure wants. The clamp is what a reader can
#' open: a forest of three studies would otherwise come out two inches tall,
#' and one of forty taller than any screen.
#'
#' @keywords internal
#' @noRd
pr_fig_size <- function(width, height, min.w = 5, max.w = 16,
                        min.h = 3, max.h = 24) {
  if (!is.finite(width))  width  <- 7
  if (!is.finite(height)) height <- 5
  c(width  = min(max(width,  min.w), max.w),
    height = min(max(height, min.h), max.h))
}

#' Open the device a grid figure is written to
#'
#' The ggplot-based figures go through `pr_write()` and `ggsave()`. The forest
#' is a grid composite rather than one plot, so it opens its own device at the
#' size it measured for itself.
#'
#' @keywords internal
#' @noRd
pr_begin_file <- function(spec, width, height) {
  if (is.null(spec)) return(NULL)
  w <- if (is.null(spec$width))  width  else spec$width
  h <- if (is.null(spec$height)) height else spec$height
  switch(spec$filetype,
         png  = grDevices::png(spec$file, width = w, height = h,
                               units = "in", res = spec$res),
         tiff = grDevices::tiff(spec$file, width = w, height = h,
                                units = "in", res = spec$res,
                                compression = "lzw"),
         pdf  = grDevices::pdf(spec$file, width = w, height = h,
                               onefile = FALSE))
  list(spec = spec, width = w, height = h)
}

#' Close the device and say where the figure went
#' @keywords internal
#' @noRd
pr_end_file <- function(con) {
  if (is.null(con)) return(invisible(NULL))
  grDevices::dev.off()
  message("easypr: wrote ", con$spec$file, "  (",
          format(round(con$width, 2)), " x ", format(round(con$height, 2)),
          " inches)")
  invisible(con$spec$file)
}
