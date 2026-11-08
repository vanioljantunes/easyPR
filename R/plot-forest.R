#' Forest plot of a prognostic meta-analysis
#'
#' One row per study, its estimate and interval drawn as a square and a line,
#' the pooled result as a diamond underneath, and the prediction interval
#' drawn through that diamond. The layout is the one `meta` and easyDR use:
#' labels on the left, the effect in the middle, the numbers on the right.
#'
#' The columns beside the effect are chosen by the `sm` the fit was made with,
#' because what is worth showing differs by measure. A validation of a model
#' is worth showing with its sample size and its events; a calibration is
#' worth showing with the observed and the expected count; a prognostic
#' factor has neither and shows the ratio alone. A column the table does not
#' carry is dropped rather than printed as a row of NA.
#'
#' @param x A fit from [easypr()].
#' @param main Title. `FALSE` draws none.
#' @param xlab Label for the effect axis. The measure's own name by default.
#' @param xlim Limits for the effect axis, on the scale you read, so
#'   `xlim = c(0.5, 4)` for a hazard ratio.
#' @param order `"data"` keeps the order of the table, `"effect"` sorts by
#'   the estimate, `"weight"` by the contribution, `"study"` alphabetically.
#' @param columns `TRUE` shows the columns `sm` chose, `FALSE` shows the
#'   study label alone, or a character vector names which of them to keep.
#' @param prediction Draw the prediction interval through the diamond.
#' @param digits Decimal places for the printed estimates.
#' @param legend Footer text. `NULL` writes the default, `FALSE` writes none.
#' @param row.height Height of one row, in inches.
#' @param effect.width Width of the effect panel, in inches.
#' @param result Print the contributions table as well as drawing.
#' @param filetype,path,file.name,width,height Where to write the figure.
#'   Leave `file.name` out to draw on the current device.
#'
#' @return Invisibly, a list with the `table` of contributions, the `pooled`
#'   result, the composite `grob`, and the `width` and `height` it occupies.
#'
#' @examples
#' \donttest{
#' data(EuroSCORE, package = "metamisc")
#' fit <- easypr(EuroSCORE, sm = "cstat")
#' easypr_forest(fit)
#' }
#' @export
easypr_forest <- function(x, main = NULL, xlab = NULL, xlim = NULL,
                          order = c("data", "effect", "weight", "study"),
                          columns = TRUE, prediction = TRUE, digits = 3,
                          legend = NULL, row.height = 0.26,
                          effect.width = 3.1, result = TRUE,
                          filetype = c("png", "pdf", "tiff"),
                          path = NULL, file.name = NULL,
                          width = NULL, height = NULL) {
  pr_check(x, "easypr_forest()")
  order <- match.arg(order)

  tab <- x$table
  tab$weight <- 100 * pr_weights(tab, x$tau2)
  tab$studlab <- pr_unique_labels(tab$studlab)
  tab <- switch(order,
                data   = tab,
                effect = tab[order(tab$est), , drop = FALSE],
                weight = tab[order(-tab$weight), , drop = FALSE],
                study  = tab[order(tab$studlab), , drop = FALSE])

  back <- pr_bwd(x$scale)
  fmt  <- function(v) format(round(back(v), digits), nsmall = digits)
  lvl  <- format(100 * x$level)

  # One row plan drives every panel, so the three of them cannot drift apart.
  mk <- function(kind, label, face = "plain", est = NA_real_, lb = NA_real_,
                 ub = NA_real_, weight = NA_real_) {
    data.frame(kind = kind, label = label, face = face, est = est, lb = lb,
               ub = ub, weight = weight, stringsAsFactors = FALSE)
  }
  plan <- rbind(
    mk("study", tab$studlab, "plain", tab$est, tab$lower, tab$upper,
       tab$weight),
    mk("pooled", "Pooled", "bold", x$estimate, x$ci.lb, x$ci.ub, 100))

  n        <- nrow(plan)
  plan$y   <- rev(seq_len(n))
  header_y <- n + 1
  het_y    <- 0
  ylim     <- c(het_y - 0.5, header_y + 0.5)
  is_study <- plan$kind == "study"
  zebra    <- pr_zebra(plan$y[is_study])

  has_est <- !is.na(plan$est)
  vals <- ifelse(!has_est, "",
                 paste0(fmt(plan$est), " (", fmt(plan$lb), " to ",
                        fmt(plan$ub), ")"))
  wts  <- ifelse(is.na(plan$weight), "",
                 paste0(format(round(plan$weight, 1), nsmall = 1), "%"))

  lab_cols  <- pr_forest_columns(x, tab, plan, columns)
  lab_right <- c(FALSE, rep(TRUE, length(lab_cols) - 1L))

  het_text <- pr_het_line(x)
  notes    <- list(list(y = het_y, label = het_text))

  p_labels <- pr_text_panel(
    lab_cols, lab_right, ypos = plan$y, face = plan$face,
    header_y = header_y, ylim = ylim, zebra = zebra, notes = notes)

  val_cols <- list(x = vals, Weight = wts)
  names(val_cols)[1L] <- paste0(x$unit, " (", lvl, "% CI)")
  p_values <- pr_text_panel(
    val_cols, c(FALSE, TRUE), ypos = plan$y, face = plan$face,
    header_y = header_y, ylim = ylim, zebra = zebra)

  # The effect panel. Squares have area proportional to weight, which is what
  # a reader reads off a square, and a diamond spans the pooled interval.
  null_at <- if (is.finite(x$null)) pr_fwd(x$scale)(x$null) else NA_real_
  lo <- min(c(plan$lb, if (prediction) x$pi.lb, null_at), na.rm = TRUE)
  hi <- max(c(plan$ub, if (prediction) x$pi.ub, null_at), na.rm = TRUE)
  if (!is.null(xlim)) {
    v  <- pr_fwd(x$scale)(xlim)
    lo <- min(v); hi <- max(v)
  }
  pad <- 0.05 * (hi - lo)
  lo  <- lo - pad; hi <- hi + pad

  pts <- plan[is_study & has_est, , drop = FALSE]
  pts$ypos <- pts$y
  pts$size <- 1.4 + 3.4 * sqrt(pts$weight / max(pts$weight, na.rm = TRUE))

  dh  <- 0.34
  dia <- plan[plan$kind == "pooled" & has_est, , drop = FALSE]
  diamond <- data.frame(
    xx = c(dia$lb[1], dia$est[1], dia$ub[1], dia$est[1]),
    yy = dia$y[1] + c(0, dh, 0, -dh))

  ticks <- pr_ticks(lo, hi, x$scale)
  keep  <- ticks$at >= lo & ticks$at <= hi
  if (is.null(xlab)) xlab <- x$label

  p_effect <- ggplot2::ggplot(pts, ggplot2::aes(x = .data$est,
                                                y = .data$ypos)) +
    zebra +
    ggplot2::geom_vline(xintercept = x$estimate, linetype = "dashed",
                        colour = "grey40", linewidth = 0.4)
  if (is.finite(null_at)) {
    p_effect <- p_effect +
      ggplot2::geom_vline(xintercept = null_at, colour = "grey25",
                          linewidth = 0.4)
  }
  p_effect <- p_effect +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$lb, xmax = .data$ub),
                           width = 0.26, orientation = "y", linewidth = 0.45) +
    ggplot2::geom_point(ggplot2::aes(size = .data$size), shape = 15L) +
    ggplot2::geom_polygon(data = diamond, inherit.aes = FALSE,
                          ggplot2::aes(x = .data$xx, y = .data$yy),
                          fill = "black")
  # The prediction interval is drawn through the diamond rather than beside
  # it, because it is the same summary read a different way, and it is the
  # one a reader of a prognostic review is supposed to quote.
  if (isTRUE(prediction) && is.finite(x$pi.lb)) {
    p_effect <- p_effect +
      ggplot2::annotate("segment", y = dia$y[1], yend = dia$y[1],
                        x = x$pi.lb, xend = x$pi.ub,
                        colour = "#b4472a", linewidth = 0.7) +
      ggplot2::annotate("point", y = c(dia$y[1], dia$y[1]),
                        x = c(x$pi.lb, x$pi.ub), shape = 124,
                        colour = "#b4472a", size = 2.4)
  }
  p_effect <- p_effect +
    ggplot2::scale_size_identity() +
    ggplot2::scale_x_continuous(breaks = ticks$at[keep],
                                labels = ticks$labels[keep]) +
    ggplot2::coord_cartesian(xlim = c(lo, hi), ylim = ylim) +
    ggplot2::labs(x = xlab) +
    pr_gg_theme(axis = TRUE)

  if (is.null(main)) {
    main <- paste0(pr_title_case(x$label), " across ", x$k, " ",
                   pr_plural(x$what, x$k))
  }
  if (identical(main, FALSE)) main <- NULL

  # The text panels take exactly the width their characters need, in inches,
  # and the effect panel takes whatever is left. Relative widths would spread
  # the columns of a narrow table across half the figure.
  lab_w <- pr_panel_width(lab_cols, notes = notes)
  val_w <- pr_panel_width(val_cols)
  body <- gridExtra::arrangeGrob(
    p_labels, p_effect, p_values, ncol = 3,
    widths = grid::unit.c(grid::unit(lab_w, "inches"),
                          grid::unit(effect.width, "inches"),
                          grid::unit(val_w, "inches")),
    padding = grid::unit(0, "line"))

  body_h <- grid::unit((ylim[2] - ylim[1]) * row.height, "inches") +
    grid::unit(0.40, "inches")
  if (is.null(legend)) legend <- pr_forest_legend(x, prediction)
  if (identical(legend, FALSE)) legend <- NULL

  total_w <- lab_w + effect.width + val_w
  if (!is.null(legend)) {
    legend <- unlist(lapply(legend, strwrap,
                            width = max(40L, floor(total_w / 0.068))))
  }
  foot_in  <- if (is.null(legend)) 0 else (1.35 * 9 * length(legend) + 8) / 72
  title_in <- if (is.null(main)) 0 else 1.9 * 12 / 72

  spec <- pr_file_spec(file.name, path, filetype, width, height)
  size <- pr_fig_size(total_w + 0.24,
                      pr_height_in(body_h) + title_in + foot_in + 0.20)
  con  <- pr_begin_file(spec, size[["width"]], size[["height"]])
  on.exit(pr_end_file(con), add = TRUE)

  g <- pr_draw_stack(body, body_h, title = main, footer = legend,
                     body_w = grid::unit(total_w, "inches"))

  out <- data.frame(
    Study    = tab$studlab,
    Estimate = fmt(tab$est),
    Lower    = fmt(tab$lower),
    Upper    = fmt(tab$upper),
    Weight   = round(tab$weight, 1),
    stringsAsFactors = FALSE)
  if (isTRUE(result)) {
    cat("Contributions to the pooled ", x$label, "\n\n", sep = "")
    print(out, row.names = FALSE)
    cat("\n", het_text, "\n", sep = "")
  }
  invisible(list(table = out,
                 pooled = list(est = x$estimate, lb = x$ci.lb, ub = x$ci.ub),
                 label = x$label, grob = g, width = total_w,
                 height = (ylim[2] - ylim[1]) * row.height + 0.40 +
                   title_in + foot_in))
}

#' The columns the labels panel carries, chosen by `sm`
#'
#' `sm` already decided which quantities the studies had to report, so it also
#' decides which of them are worth a column here. A column the table does not
#' actually carry is dropped: a sheet of hazard ratios with no sample size
#' should not grow an empty N column just because another measure would have
#' had one.
#'
#' @keywords internal
#' @noRd
pr_forest_columns <- function(x, tab, plan, columns) {
  cols <- list(Study = plan$label)
  if (isFALSE(columns)) return(cols)
  want <- names(x$columns)
  if (is.character(columns)) want <- intersect(want, columns)
  for (nm in want) {
    if (!nm %in% names(tab)) next
    v <- tab[[nm]]
    if (!any(is.finite(v))) next
    cols[[nm]] <- c(ifelse(is.finite(v), format(v, big.mark = ","), ""), "")
  }
  cols
}

#' The heterogeneity line written under the last row
#' @keywords internal
#' @noRd
pr_het_line <- function(x) {
  parts <- c(paste0("I2 = ", formatC(x$I2, format = "f", digits = 1), "%"),
             paste0("tau2 = ", formatC(x$tau2, format = "f", digits = 3),
                    " (", x$scale, " scale)"))
  if (is.finite(x$pi.lb)) {
    b <- pr_bwd(x$scale)
    parts <- c(parts, paste0(
      format(100 * x$level), "% prediction interval ",
      formatC(b(x$pi.lb), format = "f", digits = 3), " to ",
      formatC(b(x$pi.ub), format = "f", digits = 3)))
  }
  paste(parts, collapse = ", ")
}

#' What the figure says about itself
#' @keywords internal
#' @noRd
pr_forest_legend <- function(x, prediction) {
  lines <- paste0(
    "Squares are each ", x$what, "'s estimate, sized by its weight; the ",
    "diamond is the pooled ", x$label, ". Pooled on the ", x$scale,
    " scale by inverse variance, ", x$method.tau, ".")
  if (isTRUE(prediction) && is.finite(x$pi.lb)) {
    lines <- c(lines, paste0(
      "The red bar through the diamond is the ", format(100 * x$level),
      "% prediction interval: what the next ", x$what,
      " should expect, and the one to quote."))
  }
  lines
}

#' Capitalise the first letter, and nothing else
#' @keywords internal
#' @noRd
pr_title_case <- function(s) {
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}
