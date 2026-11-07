#' @export
print.easypr_result <- function(x, digits = 3, ...) {
  unit <- if (isTRUE(x$log)) "studies" else "validations"
  cat("Pooled ", x$label, " across ", x$k, " ", unit, " (",
      x$method, ")\n\n", sep = "")
  # a ratio was pooled on the log scale, so it is exponentiated to print
  b <- if (isTRUE(x$log)) exp else identity
  f <- function(v) formatC(b(v), format = "f", digits = digits)
  cat("  estimate            ", f(x$estimate), "\n", sep = "")
  cat("  ", round(100 * x$level), "% confidence     ", f(x$ci.lb), " to ",
      f(x$ci.ub), "\n", sep = "")
  if (is.finite(x$pi.lb)) {
    cat("  ", round(100 * x$level), "% prediction     ", f(x$pi.lb), " to ",
        f(x$pi.ub), "\n", sep = "")
  }
  # tau-squared is a variance on the scale the pooling happened on, so it is
  # never exponentiated even when the effect is
  cat("  tau-squared         ",
      formatC(x$tau2, format = "f", digits = digits),
      if (isTRUE(x$log)) "  (log scale)" else "", "\n", sep = "")
  cat("  I-squared           ", formatC(x$I2, format = "f", digits = 1),
      "%\n\n", sep = "")
  cat(strwrap(pr_reading(x), width = 76), sep = "\n")
  invisible(x)
}

#' The sentence the numbers are for
#'
#' A pooled c-statistic with a wide prediction interval is the common result
#' in this literature and the one most often reported as if it were narrow,
#' so the print method says which interval to read rather than leaving it to
#' be inferred.
#'
#' @keywords internal
#' @noRd
pr_reading <- function(x) {
  if (!is.finite(x$pi.lb)) {
    return(paste0("Too few validations for a prediction interval. The ",
                  "confidence interval below is about the average, not ",
                  "about the next hospital."))
  }
  wide <- (x$pi.ub - x$pi.lb) > 2 * (x$ci.ub - x$ci.lb)
  what <- if (isTRUE(x$log)) "effect" else "performance"
  base <- paste0("The confidence interval is about the average ", what, ". ",
                 "The prediction interval is what a new validation should ",
                 "expect, and it is the one to quote.")
  if (!wide) return(base)
  tail <- if (isTRUE(x$log))
    " Here it is much the wider of the two, so the effect is not settled: where the next study is run matters as much as the factor."
  else
    " Here it is much the wider of the two, so the model's performance is not settled: where it is next validated matters as much as the model."
  paste0(base, tail)
}

#' @export
summary.easypr_result <- function(object, ...) {
  structure(list(result = object, table = object$table),
            class = "summary.easypr_result")
}

#' @export
print.summary.easypr_result <- function(x, digits = 3, ...) {
  print(x$result, digits = digits)
  cat("\n\nPer ", if (isTRUE(x$result$log)) "study" else "validation",
      "\n\n", sep = "")
  t <- x$table
  bb <- if (isTRUE(x$result$log)) exp else identity
  out <- data.frame(
    Study = t$studlab,
    Estimate = formatC(bb(t$est), format = "f", digits = digits),
    `95% CI` = paste0(formatC(bb(t$lower), format = "f", digits = digits), " to ",
                      formatC(bb(t$upper), format = "f", digits = digits)),
    Weight = paste0(formatC(100 * pr_weights(t), format = "f", digits = 1), "%"),
    check.names = FALSE
  )
  print(out, row.names = FALSE)
  invisible(x)
}

#' Random-effects weights, as a share of one
#' @keywords internal
#' @noRd
pr_weights <- function(tab, tau2 = 0) {
  w <- 1 / (tab$se^2 + tau2)
  w / sum(w)
}
