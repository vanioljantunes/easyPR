#' @export
print.easypr_result <- function(x, digits = 3, ...) {
  unit <- pr_plural(x$what, x$k)
  cat("Pooled ", x$label, " across ", x$k, " ", unit, "\n", sep = "")
  cat("Pooling: inverse variance, random effects, ", x$method.tau, ", ",
      if (identical(x$method.ci, "t")) "Knapp-Hartung interval"
      else "normal interval", ".\n\n", sep = "")
  # everything inside the object is on the pooling scale, so this is the one
  # place the reader's scale is restored
  b <- pr_bwd(x$scale)
  f <- function(v) formatC(b(v), format = "f", digits = digits)
  pad <- function(s) formatC(s, width = -20)
  cat("  ", pad(x$unit), f(x$estimate), "\n", sep = "")
  cat("  ", pad(paste0(round(100 * x$level), "% confidence")),
      f(x$ci.lb), " to ", f(x$ci.ub), "\n", sep = "")
  if (is.finite(x$pi.lb)) {
    cat("  ", pad(paste0(round(100 * x$level), "% prediction")),
        f(x$pi.lb), " to ", f(x$pi.ub), "\n", sep = "")
  }
  if (isTRUE(x$common)) {
    cat("  ", pad("fixed effect"), f(x$fixed$estimate), " (",
        f(x$fixed$ci.lb), " to ", f(x$fixed$ci.ub), ")\n", sep = "")
  }
  # tau-squared is a variance on the scale the pooling happened on, so it is
  # never exponentiated even when the effect is
  cat("  ", pad("tau-squared"),
      formatC(x$tau2, format = "f", digits = digits),
      "  (", x$scale, " scale)\n", sep = "")
  cat("  ", pad("I-squared"), formatC(x$I2, format = "f", digits = 1),
      "%\n", sep = "")
  cat("  ", pad("test of effect"), x$test$name, " = ",
      formatC(x$test$stat, format = "f", digits = 2), ", ",
      pr_p_lab(x$test$p), "\n\n", sep = "")
  cat(strwrap(pr_reading(x), width = 76), sep = "\n")
  invisible(x)
}

#' The sentence the numbers are for
#'
#' A pooled performance with a wide prediction interval is the common result
#' in this literature and the one most often reported as if it were narrow,
#' so the print method says which interval to read rather than leaving it to
#' be inferred.
#'
#' @keywords internal
#' @noRd
pr_reading <- function(x) {
  if (!is.finite(x$pi.lb)) {
    return(paste0("Too few ", pr_plural(x$what), " for a prediction interval. The ",
                  "confidence interval below is about the average, not ",
                  "about the next one."))
  }
  wide <- (x$pi.ub - x$pi.lb) > 2 * (x$ci.ub - x$ci.lb)
  what <- if (identical(x$measure, "ratio")) "effect" else "performance"
  base <- paste0("The confidence interval is about the average ", what, ". ",
                 "The prediction interval is what a new ", x$what,
                 " should expect, and it is the one to quote.")
  if (!wide) return(base)
  tail <- if (identical(x$measure, "ratio"))
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
  cat("\n\nPer ", x$result$what, "\n\n", sep = "")
  t <- x$table
  bb <- pr_bwd(x$result$scale)
  out <- data.frame(
    Study = t$studlab,
    Estimate = formatC(bb(t$est), format = "f", digits = digits),
    `95% CI` = paste0(formatC(bb(t$lower), format = "f", digits = digits), " to ",
                      formatC(bb(t$upper), format = "f", digits = digits)),
    Weight = paste0(formatC(100 * pr_weights(t), format = "f", digits = 1), "%"),
    check.names = FALSE
  )
  # the columns sm asked for sit beside the effect, as they do in the forest
  for (nm in names(x$result$columns)) {
    if (nm %in% names(t) && any(is.finite(t[[nm]]))) {
      out[[nm]] <- format(t[[nm]])
    }
  }
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
