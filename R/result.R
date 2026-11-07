#' @export
print.easypr_result <- function(x, digits = 3, ...) {
  cat("Pooled ", x$label, " across ", x$k, " validations (",
      x$method, ")\n\n", sep = "")
  f <- function(v) formatC(v, format = "f", digits = digits)
  cat("  estimate            ", f(x$estimate), "\n", sep = "")
  cat("  ", round(100 * x$level), "% confidence     ", f(x$ci.lb), " to ",
      f(x$ci.ub), "\n", sep = "")
  if (is.finite(x$pi.lb)) {
    cat("  ", round(100 * x$level), "% prediction     ", f(x$pi.lb), " to ",
        f(x$pi.ub), "\n", sep = "")
  }
  cat("  tau-squared         ", f(x$tau2), "\n", sep = "")
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
  base <- paste0("The confidence interval is about the average performance. ",
                 "The prediction interval is what a new validation should ",
                 "expect, and it is the one to quote.")
  if (!wide) return(base)
  paste0(base, " Here it is much the wider of the two, so the model's ",
         "performance is not settled: where it is next validated matters ",
         "as much as the model.")
}

#' @export
summary.easypr_result <- function(object, ...) {
  structure(list(result = object, table = object$table),
            class = "summary.easypr_result")
}

#' @export
print.summary.easypr_result <- function(x, digits = 3, ...) {
  print(x$result, digits = digits)
  cat("\n\nPer validation\n\n")
  t <- x$table
  out <- data.frame(
    Study = t$studlab,
    Estimate = formatC(t$est, format = "f", digits = digits),
    `95% CI` = paste0(formatC(t$lower, format = "f", digits = digits), " to ",
                      formatC(t$upper, format = "f", digits = digits)),
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
