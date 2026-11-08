#' Leave one study out
#'
#' Refits the pool with each study removed in turn, and draws the refits the
#' way the forest draws the studies. Every row is one analysis rather than
#' one study, so it carries that analysis's own pooled estimate, interval,
#' heterogeneity and test. The last row, in bold, is the analysis with
#' nothing left out, which is what the others are read against.
#'
#' A pooled result that moves when one study leaves is being carried by that
#' study, and a reader is entitled to know which one before they trust the
#' number. It is a sensitivity analysis to declare, not a licence to drop the
#' study.
#'
#' @inheritParams easypr_forest
#' @param order `"effect"` sorts the refits by their estimate, which puts the
#'   study that moves the result at one end; `"data"` keeps the table's
#'   order; `"study"` sorts by the omitted study's name.
#' @return Invisibly, the table of refits with the composite grob.
#' @examples
#' \donttest{
#' data(EuroSCORE, package = "metamisc")
#' easypr_loo(easypr(EuroSCORE, sm = "cstat"))
#' }
#' @export
easypr_loo <- function(x, label.e = NULL, label.c = NULL,
                       direction = c("bad", "good"),
                       main = NULL, xlab = NULL, xlim = NULL,
                       order = c("effect", "data", "study"),
                       digits = 3, legend = NULL, row.height = 0.26,
                       effect.width = 3.6, result = TRUE,
                       filetype = c("png", "pdf", "tiff"), path = NULL,
                       file.name = NULL, width = NULL, height = NULL) {
  pr_check(x, "easypr_loo()")
  order     <- match.arg(order)
  direction <- match.arg(direction)
  t <- x$table
  if (nrow(t) < 3L) {
    pr_stop("leave-one-out needs at least three ", pr_plural(x$what), ".")
  }
  t$studlab <- pr_unique_labels(t$studlab)

  refits <- do.call(rbind, lapply(seq_len(nrow(t)), function(i) {
    r <- pr_rma(t[-i, ], x$method.tau, x$method.ci)
    data.frame(omitted = t$studlab[i], est = r$est, lower = r$ci.lb,
               upper = r$ci.ub, i2 = r$I2, p = r$p,
               stringsAsFactors = FALSE)
  }))
  refits <- switch(order,
                   effect = refits[order(refits$est), , drop = FALSE],
                   data   = refits,
                   study  = refits[order(refits$omitted), , drop = FALSE])

  back <- pr_bwd(x$scale)
  fmt  <- function(v) format(round(back(v), digits), nsmall = digits)
  lvl  <- format(100 * x$level)

  mk <- function(kind, label, face, est, lb, ub) {
    data.frame(kind = kind, label = label, face = face, est = est, lb = lb,
               ub = ub, weight = NA_real_, stringsAsFactors = FALSE)
  }
  plan <- rbind(
    mk("point", refits$omitted, "plain", refits$est, refits$lower,
       refits$upper),
    mk("pooled", "None omitted", "bold", x$estimate, x$ci.lb, x$ci.ub))

  n        <- nrow(plan)
  plan$y   <- rev(seq_len(n))
  header_y <- n + 1
  span_y   <- n + 2
  ylim     <- c(-0.6, span_y + 1.5)
  zebra    <- pr_zebra(plan$y[plan$kind == "point"])

  all_i2 <- c(refits$i2, x$I2)
  all_p  <- c(refits$p, if (is.null(x$test)) NA_real_ else x$test$p)
  val_cols <- list(
    x    = fmt(plan$est),
    y    = paste0("(", fmt(plan$lb), " to ", fmt(plan$ub), ")"),
    `I2` = paste0(formatC(all_i2, format = "f", digits = 1), "%"),
    `p`  = vapply(all_p, pr_p, character(1)))
  names(val_cols)[1:2] <- c(x$unit, paste0(lvl, "% CI"))

  lab_cols <- list(Omitted = plan$label)

  out <- pr_forest_draw(
    plan = plan, lab_cols = lab_cols, lab_just = "left", span_y = span_y,
    val_cols = val_cols, val_just = "center", val_colour = NULL,
    x = x, xlab = if (is.null(xlab)) paste("Pooled", x$label) else xlab,
    xlim = xlim, prediction = FALSE,
    label.e = label.e, label.c = label.c, direction = direction,
    header_y = header_y, ylim = ylim, zebra = zebra, notes = NULL,
    row.height = row.height, effect.width = effect.width,
    main = main, legend = legend,
    filetype = filetype, path = path, file.name = file.name,
    width = width, height = height)

  tbl <- data.frame(
    Omitted  = plan$label,
    Estimate = fmt(plan$est),
    `95% CI` = paste0(fmt(plan$lb), " to ", fmt(plan$ub)),
    `I2 (%)` = formatC(all_i2, format = "f", digits = 1),
    `p`      = vapply(all_p, pr_p, character(1)),
    check.names = FALSE, stringsAsFactors = FALSE)
  if (isTRUE(result)) {
    cat("Leave one ", x$what, " out, pooled ", x$label, "\n\n", sep = "")
    print(tbl, row.names = FALSE)
    cat("\n", pr_loo_shift(x, refits), "\n", sep = "")
  }
  invisible(c(list(table = tbl), out))
}

#' One line saying whether anything moved
#' @keywords internal
#' @noRd
pr_loo_shift <- function(x, refits) {
  b <- pr_bwd(x$scale)
  d <- abs(b(refits$est) - b(x$estimate))
  i <- which.max(d)
  sprintf("Largest shift %.3f, when %s is omitted; nothing omitted, %.3f.",
          d[i], refits$omitted[i], b(x$estimate))
}

#' A plain random-effects pool of an estimate and standard error column
#'
#' metafor rather than valmeta, because the leave-one-out loop refits the
#' same already-reconciled estimates many times and does not need the
#' reconstruction step again. The estimator and the interval method are the
#' ones the original fit used, or every refit would answer a slightly
#' different question from the fit it is compared against.
#'
#' @keywords internal
#' @noRd
pr_rma <- function(tab, method.tau = "REML", method.ci = "t") {
  test <- if (identical(method.ci, "t")) "knha" else "z"
  f <- metafor::rma(yi = tab$est, sei = tab$se, method = method.tau,
                    test = test)
  list(est = as.numeric(f$b), ci.lb = f$ci.lb, ci.ub = f$ci.ub,
       I2 = f$I2, tau2 = f$tau2, p = as.numeric(f$pval)[1])
}
