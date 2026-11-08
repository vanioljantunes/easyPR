#' Forest plot of a prognostic meta-analysis
#'
#' One row per study, its estimate and interval drawn as a square and a line,
#' the pooled result as a diamond underneath, and the prediction interval in
#' red beneath the pooled interval. The layout is the one `meta` uses: the
#' labels on the left, the effect in the middle, the numbers on the right.
#'
#' The columns beside the effect are chosen by the `sm` the fit was made
#' with, because what is worth showing differs by measure. A validation of a
#' model is worth showing with its sample size and its events; a calibration
#' with the observed and the expected count; a prognostic factor with the
#' sample size alone. A column the table does not carry is dropped rather
#' than printed as a row of blanks.
#'
#' @param x A fit from [easypr()].
#' @param label.e,label.c Names of the two groups being compared. They head
#'   the columns beside the study label, and they name the two sides of the
#'   axis.
#' @param direction `"bad"` when a higher value of the outcome is worse,
#'   which is the usual case in prognosis, `"good"` when it is better. It
#'   decides which side of the axis each group is said to be favoured on.
#' @param main Title. The default is none.
#' @param xlab Label under the axis, set in bold. The measure's own name by
#'   default.
#' @param xlim Limits for the effect axis, on the scale you read, so
#'   `xlim = c(0.5, 4)` for a hazard ratio.
#' @param order `"data"` keeps the order of the table, `"effect"` sorts by
#'   the estimate, `"weight"` by the contribution, `"study"` alphabetically.
#' @param columns `TRUE` shows the columns `sm` chose, `FALSE` shows the
#'   study label alone, or a character vector names which of them to keep.
#' @param prediction Draw the prediction interval.
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
#' easypr.object.cstat <- easypr(EuroSCORE, sm = "cstat")
#' easypr_forest(easypr.object.cstat)
#' }
#' @export
easypr_forest <- function(x, label.e = NULL, label.c = NULL,
                          direction = c("bad", "good"),
                          main = NULL, xlab = NULL, xlim = NULL,
                          order = c("data", "effect", "weight", "study"),
                          columns = TRUE, prediction = TRUE, digits = 3,
                          legend = NULL, row.height = 0.26,
                          effect.width = 3.6, result = TRUE,
                          filetype = c("png", "pdf", "tiff"),
                          path = NULL, file.name = NULL,
                          width = NULL, height = NULL) {
  pr_check(x, "easypr_forest()")
  order     <- match.arg(order)
  direction <- match.arg(direction)

  tab <- x$table
  tab$weight  <- 100 * pr_weights(tab, x$tau2)
  tab$studlab <- pr_unique_labels(tab$studlab)
  tab <- switch(order,
                data   = tab,
                effect = tab[order(tab$est), , drop = FALSE],
                weight = tab[order(-tab$weight), , drop = FALSE],
                study  = tab[order(tab$studlab), , drop = FALSE])

  back <- pr_bwd(x$scale)
  fmt  <- function(v) format(round(back(v), digits), nsmall = digits)
  lvl  <- format(100 * x$level)
  has_pi <- isTRUE(prediction) && is.finite(x$pi.lb)

  # The row plan drives all three panels, so they cannot drift apart. The
  # prediction is a row of its own, carrying no square and no diamond: it is
  # written in red under the pooled interval and nowhere else.
  mk <- function(kind, label, face = "plain", est = NA_real_, lb = NA_real_,
                 ub = NA_real_, weight = NA_real_) {
    data.frame(kind = kind, label = label, face = face, est = est, lb = lb,
               ub = ub, weight = weight, stringsAsFactors = FALSE)
  }
  plan <- rbind(
    mk("study", tab$studlab, "plain", tab$est, tab$lower, tab$upper,
       tab$weight),
    mk("pooled", "Pooled", "bold", x$estimate, x$ci.lb, x$ci.ub, 100))
  if (has_pi) plan <- rbind(plan, mk("pred", "Prediction interval", "bold"))

  n        <- nrow(plan)
  plan$y   <- rev(seq_len(n))
  header_y <- n + 1
  note_y   <- c(0, -1)
  ylim     <- c(min(note_y) - 0.8, header_y + 0.9)
  is_study <- plan$kind == "study"
  zebra    <- pr_zebra(plan$y[is_study])

  # The effect and its interval are two columns, so a reader can read down
  # either one on its own.
  est_txt <- ifelse(is.na(plan$est), "", fmt(plan$est))
  ci_txt  <- ifelse(is.na(plan$lb), "",
                    paste0("(", fmt(plan$lb), " to ", fmt(plan$ub), ")"))
  wt_txt  <- ifelse(is.na(plan$weight), "",
                    paste0(format(round(plan$weight, 1), nsmall = 1), "%"))
  if (has_pi) {
    i <- which(plan$kind == "pred")
    ci_txt[i] <- paste0("(", fmt(x$pi.lb), " to ", fmt(x$pi.ub), ")")
  }
  val_cols <- list(x = est_txt, y = ci_txt, Weight = wt_txt)
  names(val_cols)[1:2] <- c(x$unit, paste0(lvl, "% CI"))

  lab <- pr_forest_columns(x, tab, plan, columns)

  notes <- list(
    list(y = note_y[1], label = pr_effect_test(x)),
    list(y = note_y[2], label = pr_het_line(x)))

  out <- pr_forest_draw(
    plan = plan, lab_cols = lab$cols, lab_just = lab$just,
    val_cols = val_cols, val_just = "center", val_colour = NULL,
    x = x, xlab = xlab, xlim = xlim, prediction = has_pi,
    label.e = label.e, label.c = label.c, direction = direction,
    header_y = header_y, ylim = ylim, zebra = zebra, notes = notes,
    row.height = row.height, effect.width = effect.width,
    main = main, legend = legend,
    filetype = filetype, path = path, file.name = file.name,
    width = width, height = height)

  tbl <- data.frame(
    Study    = tab$studlab,
    Estimate = fmt(tab$est),
    Lower    = fmt(tab$lower),
    Upper    = fmt(tab$upper),
    Weight   = round(tab$weight, 1),
    stringsAsFactors = FALSE)
  if (isTRUE(result)) {
    cat("Contributions to the pooled ", x$label, "\n\n", sep = "")
    print(tbl, row.names = FALSE)
    cat("\n", pr_effect_test(x), "\n", pr_het_line(x), "\n", sep = "")
  }
  invisible(c(list(table = tbl,
                   pooled = list(est = x$estimate, lb = x$ci.lb,
                                 ub = x$ci.ub),
                   label = x$label), out))
}


# ---------------------------------------------------------------------------
# The body both the forest and the leave-one-out are drawn with.

#' Compose and draw the three panels
#'
#' Everything that is the same between a forest and a leave-one-out lives
#' here: the panels, their widths, the axis, the method line over it and the
#' two sides of it, so the two figures cannot drift apart.
#'
#' @keywords internal
#' @noRd
pr_forest_draw <- function(plan, lab_cols, lab_just,
                           val_cols, val_just, val_colour, x, xlab, xlim,
                           prediction, label.e, label.c, direction,
                           header_y, ylim, zebra, notes, row.height,
                           effect.width, main, legend,
                           filetype, path, file.name, width, height) {
  p_labels <- pr_text_panel(
    lab_cols, lab_just, ypos = plan$y, face = plan$face,
    header_y = header_y, ylim = ylim, zebra = zebra, notes = notes)

  p_values <- pr_text_panel(
    val_cols, val_just, ypos = plan$y, face = plan$face,
    header_y = header_y, ylim = ylim, zebra = zebra, colour = val_colour)

  fwd     <- pr_fwd(x$scale)
  null_at <- if (is.finite(x$null)) fwd(x$null) else NA_real_
  lo <- min(c(plan$lb, if (prediction) x$pi.lb, null_at), na.rm = TRUE)
  hi <- max(c(plan$ub, if (prediction) x$pi.ub, null_at), na.rm = TRUE)
  if (!is.null(xlim)) {
    v <- fwd(xlim); lo <- min(v); hi <- max(v)
  }
  pad <- 0.05 * (hi - lo)
  lo  <- lo - pad; hi <- hi + pad

  has_est  <- !is.na(plan$est)
  squares  <- plan[plan$kind == "study" & has_est, , drop = FALSE]
  points   <- plan[plan$kind == "point" & has_est, , drop = FALSE]
  diamonds <- plan[plan$kind %in% c("pooled", "summary") & has_est, ,
                   drop = FALSE]

  p <- ggplot2::ggplot() + zebra
  if (is.finite(null_at)) {
    p <- p + ggplot2::geom_vline(xintercept = null_at, colour = "grey25",
                                 linewidth = 0.4)
  }
  if (nrow(diamonds) > 0L) {
    p <- p + ggplot2::geom_vline(xintercept = diamonds$est[nrow(diamonds)],
                                 linetype = "dashed", colour = "grey40",
                                 linewidth = 0.4)
  }
  if (nrow(squares) > 0L) {
    squares$size <- 1.4 + 3.4 * sqrt(squares$weight /
                                       max(squares$weight, na.rm = TRUE))
    p <- p +
      ggplot2::geom_errorbar(
        data = squares,
        ggplot2::aes(y = .data$y, xmin = .data$lb, xmax = .data$ub),
        width = 0.26, orientation = "y", linewidth = 0.45) +
      ggplot2::geom_point(
        data = squares,
        ggplot2::aes(x = .data$est, y = .data$y, size = .data$size),
        shape = 15L) +
      ggplot2::scale_size_identity()
  }
  if (nrow(points) > 0L) {
    p <- p +
      ggplot2::geom_errorbar(
        data = points,
        ggplot2::aes(y = .data$y, xmin = .data$lb, xmax = .data$ub),
        width = 0.22, orientation = "y", linewidth = 0.45,
        colour = "grey40") +
      ggplot2::geom_point(
        data = points, ggplot2::aes(x = .data$est, y = .data$y),
        size = 1.9, colour = "#1b3a5c")
  }
  for (i in seq_len(nrow(diamonds))) {
    d <- diamonds[i, ]
    p <- p + ggplot2::geom_polygon(
      data = data.frame(xx = c(d$lb, d$est, d$ub, d$est),
                        yy = d$y + c(0, 0.34, 0, -0.34)),
      ggplot2::aes(x = .data$xx, y = .data$yy), fill = "black")
  }
  pred_row <- plan[plan$kind == "pred", , drop = FALSE]
  if (isTRUE(prediction) && nrow(pred_row) > 0L) {
    py <- pred_row$y[1]
    p <- p +
      ggplot2::annotate("segment", y = py, yend = py,
                        x = x$pi.lb, xend = x$pi.ub, linewidth = 0.7) +
      ggplot2::annotate("point", y = c(py, py), x = c(x$pi.lb, x$pi.ub),
                        shape = 124, size = 2.4)
  }

  # Centred over the scale: how the interval was built, how the between-study
  # variance was estimated, and what is being pooled.
  p <- p + ggplot2::annotate("text", x = (lo + hi) / 2, y = header_y,
                             label = pr_method_line(x), size = 10 / 2.845,
                             colour = "black", fontface = "bold",
                             lineheight = 0.9)

  fav   <- pr_favours(label.e, label.c, direction)
  ticks <- pr_ticks(lo, hi, x$scale)
  keep  <- ticks$at >= lo & ticks$at <= hi
  if (is.null(xlab)) xlab <- x$label
  # The favours labels and the outcome name are one block, drawn under the
  # panels rather than by the axis, so they share an ink and a size and the
  # favours line can sit between the tick labels and the outcome.
  axis_blk <- pr_axis_grob(xlab, fav)
  p_effect <- p +
    ggplot2::scale_x_continuous(breaks = ticks$at[keep],
                                labels = ticks$labels[keep]) +
    ggplot2::coord_cartesian(xlim = c(lo, hi), ylim = ylim, clip = "off",
                             expand = FALSE) +
    pr_gg_theme(axis = TRUE, title = FALSE)

  if (identical(main, FALSE)) main <- NULL
  lab_w <- pr_panel_width(lab_cols)
  val_w <- pr_panel_width(val_cols)
  # The labels panel is added last so that it is drawn last. A note is wider
  # than the column it starts in, and whichever panel is drawn after it
  # covers the overflow with its own background.
  panels <- gridExtra::arrangeGrob(
    grobs = list(p_effect, p_values, p_labels),
    layout_matrix = matrix(c(3L, 1L, 2L), nrow = 1L),
    widths = grid::unit.c(grid::unit(lab_w, "inches"),
                          grid::unit(effect.width, "inches"),
                          grid::unit(val_w, "inches")),
    padding = grid::unit(0, "line"))

  panels_h <- grid::unit((ylim[2] - ylim[1]) * row.height, "inches") +
    grid::unit(0.52, "inches")
  if (is.null(axis_blk)) {
    body <- panels
    body_h <- panels_h
  } else {
    # The axis block is as wide as the effect panel and sits under it.
    axis_row <- gridExtra::arrangeGrob(
      grobs = list(grid::nullGrob(), axis_blk$grob, grid::nullGrob()),
      nrow = 1L,
      widths = grid::unit.c(grid::unit(lab_w, "inches"),
                            grid::unit(effect.width, "inches"),
                            grid::unit(val_w, "inches")))
    body <- gridExtra::arrangeGrob(
      grobs = list(panels, axis_row), ncol = 1L,
      heights = grid::unit.c(panels_h, axis_blk$height))
    body_h <- panels_h + axis_blk$height
  }
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
  list(grob = g, width = total_w,
       height = pr_height_in(body_h) + title_in + foot_in)
}

#' The columns the labels panel carries, and the groups that head them
#'
#' `sm` already decided which quantities the studies had to report, so it
#' also decides which of them are worth a column here. The group names span
#' those columns and never the study label: with two of them the first takes
#' `label.e` and the second `label.c`, with one `label.e` spans it alone.
#'
#' @keywords internal
#' @noRd
pr_forest_columns <- function(x, tab, plan, columns) {
  cols <- list(Study = plan$label)
  if (isFALSE(columns)) {
    return(list(cols = cols, just = "left"))
  }
  want <- names(x$columns)
  if (is.character(columns)) want <- intersect(want, columns)
  for (nm in want) {
    if (!nm %in% names(tab)) next
    v <- tab[[nm]]
    if (!any(is.finite(v))) next
    # A column of counts is totalled on the summary row, the way {meta}
    # totals the events and the sample size.
    body <- ifelse(is.finite(v), format(v, big.mark = ","), "")
    tail <- rep("", nrow(plan) - nrow(tab))
    if (length(tail) > 0L) {
      tail[1] <- format(sum(v, na.rm = TRUE), big.mark = ",")
    }
    cols[[nm]] <- c(body, tail)
  }
  list(cols = cols, just = c("left", rep("center", length(cols) - 1L)))
}

#' Which group each side of the axis favours
#'
#' In prognosis the outcome is usually something bad, so an estimate above
#' the null means the first group does worse and the axis says so. A good
#' outcome turns both labels around.
#'
#' @keywords internal
#' @noRd
pr_favours <- function(label.e, label.c, direction) {
  if (is.null(label.e) && is.null(label.c)) return(NULL)
  e  <- if (is.null(label.e)) "the first group" else label.e
  c. <- if (is.null(label.c)) "the second group" else label.c
  if (identical(direction, "bad")) {
    list(left = paste("Favours", e), right = paste("Favours", c.))
  } else {
    list(left = paste("Favours", c.), right = paste("Favours", e))
  }
}

#' The line written over the scale
#' @keywords internal
#' @noRd
pr_method_line <- function(x) {
  ci <- if (identical(x$method.ci, "t")) "Knapp-Hartung interval"
        else "normal interval"
  paste0(x$method, ", ", ci, ",\n", x$method.tau, ", ", x$sm)
}

#' The test of the pooled effect, written under it
#' @keywords internal
#' @noRd
pr_effect_test <- function(x) {
  if (is.null(x$test) || !is.finite(x$test$p)) return("")
  paste0("Test for overall effect ", pr_p_lab(x$test$p))
}

#' The heterogeneity line, written under the test
#' @keywords internal
#' @noRd
pr_het_line <- function(x) {
  paste0("Heterogeneity: I2 = ", formatC(x$I2, format = "f", digits = 1),
         "%, tau2 = ", formatC(x$tau2, format = "f", digits = 3),
         " (", x$scale, " scale)")
}
