#' Leave one validation out
#'
#' Refits the pool with each validation removed in turn. A pooled performance
#' that moves when one study leaves is being carried by that study, and a
#' reader is entitled to know which one before they trust the number.
#'
#' @inheritParams easypr_forest
#' @return The ggplot, invisibly. The per-study refits are attached as the
#'   `"table"` attribute.
#' @examples
#' \donttest{
#' data(EuroSCORE, package = "metamisc")
#' easypr_loo(easypr(EuroSCORE, measure = "cstat"))
#' }
#' @export
easypr_loo <- function(x, main = NULL, xlab = NULL, result = TRUE,
                       filetype = c("png", "pdf", "tiff"), path = NULL,
                       file.name = NULL, width = NULL, height = NULL) {
  pr_check(x, "easypr_loo()")
  t <- x$table
  if (nrow(t) < 3L) pr_stop("leave-one-out needs at least three validations.")

  out <- do.call(rbind, lapply(seq_len(nrow(t)), function(i) {
    r <- pr_rma(t[-i, ])
    data.frame(omitted = t$studlab[i], est = r$est,
               lower = r$ci.lb, upper = r$ci.ub, i2 = r$I2,
               stringsAsFactors = FALSE)
  }))
  out <- out[order(out$est), ]
  out$omitted <- factor(pr_unique_labels(out$omitted),
                        levels = pr_unique_labels(out$omitted))

  if (is.null(xlab)) xlab <- paste("Pooled", x$label, "with the study omitted")
  if (is.null(main)) main <- "Leave one validation out"

  p <- ggplot2::ggplot(out, ggplot2::aes(x = est, y = omitted)) +
    ggplot2::annotate("rect", xmin = x$ci.lb, xmax = x$ci.ub, ymin = -Inf,
                      ymax = Inf, alpha = 0.12, fill = "#1b3a5c") +
    ggplot2::geom_vline(xintercept = x$estimate, colour = "#1b3a5c") +
    # ggplot2 3.5 renamed the cap size of a horizontal error bar from
    # height to width, and warns on every plot while it translates the old
    # name, so the new one is written here.
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper),
                            width = 0, colour = "grey45") +
    ggplot2::geom_point(colour = "#1b3a5c", size = 2) +
    ggplot2::labs(x = xlab, y = NULL, title = main,
                  subtitle = pr_loo_sub(x, out)) +
    pr_theme()

  if (result) {
    cat("Leave-one-out, pooled ", x$label, "\n\n", sep = "")
    print(data.frame(Omitted = as.character(out$omitted),
                     Pooled = formatC(out$est, format = "f", digits = 3),
                     `95% CI` = paste0(formatC(out$lower, format = "f", digits = 3),
                                       " to ",
                                       formatC(out$upper, format = "f", digits = 3)),
                     `I2 (%)` = formatC(out$i2, format = "f", digits = 0),
                     check.names = FALSE), row.names = FALSE)
    cat("\nAll studies in: ", formatC(x$estimate, format = "f", digits = 3), "\n",
        sep = "")
  }
  spec <- pr_file_spec(file.name, path, filetype, width, height)
  pr_write(p, spec, width = 8, height = max(3.2, 0.26 * nrow(out) + 2))
  if (is.null(spec)) print(p)
  invisible(structure(p, table = out))
}

#' One line saying whether anything moved
#' @keywords internal
#' @noRd
pr_loo_sub <- function(x, out) {
  d <- max(abs(out$est - x$estimate))
  who <- as.character(out$omitted[which.max(abs(out$est - x$estimate))])
  sprintf("Largest shift %.3f, when %s is dropped; all studies in, %.3f",
          d, who, x$estimate)
}

#' A plain random-effects pool of an estimate and standard error column
#'
#' metafor rather than valmeta, because the leave-one-out loop refits the
#' same already-reconciled estimates many times and does not need the
#' reconstruction step again.
#'
#' @keywords internal
#' @noRd
pr_rma <- function(tab) {
  f <- metafor::rma(yi = tab$est, sei = tab$se, method = "REML")
  list(est = as.numeric(f$b), ci.lb = f$ci.lb, ci.ub = f$ci.ub,
       I2 = f$I2, tau2 = f$tau2)
}
