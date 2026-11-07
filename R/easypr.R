#' Pool the performance of a prognostic model across its validations
#'
#' One call takes a table of external validations and the measure you are
#' pooling, and returns the pooled performance with its heterogeneity and a
#' prediction interval.
#'
#' Which measure you ask for is a question about the model, not about the
#' data. `"cstat"` pools discrimination: can the model tell who is at higher
#' risk from who is at lower risk. `"OE"` pools calibration-in-the-large: are
#' the risks it quotes the risks that actually happen. The two can point in
#' different directions, and a review that reports only the first is quietly
#' leaving out whether the numbers mean anything.
#'
#' The prediction interval is the one to read. A confidence interval says
#' where the average performance sits; a prediction interval says what the
#' next hospital should expect when they run the model on their own patients,
#' which is the question a reader of a prognostic review actually has.
#'
#' Estimation is `metamisc::valmeta()`. easyPR works out each study's estimate
#' and standard error first, from whichever quantities it reported, so the
#' same call works whether the papers gave confidence intervals, standard
#' errors, or only counts.
#'
#' @param data A data frame, one row per validation.
#' @param measure `"cstat"` for the c-statistic, `"OE"` for the
#'   observed-to-expected ratio.
#' @param studlab Column of `data` holding the study labels, or a vector of
#'   them. Left alone, easyPR looks for a `Study` column.
#' @param method Pooling method passed to `metamisc::valmeta()`, `"REML"` by
#'   default.
#' @param level Width of the confidence and prediction intervals.
#' @param ... Passed on to `metamisc::valmeta()`.
#'
#' @return An object of class `easypr_result`: a list with the pooled
#'   `estimate`, its interval, the prediction interval, `tau2`, `I2`, the
#'   per-study `table`, and the `valmeta` fit it came from.
#'
#' @examples
#' \donttest{
#' data(EuroSCORE, package = "metamisc")
#' fit <- easypr(EuroSCORE, measure = "cstat")
#' fit
#' }
#' @export
easypr <- function(data, measure = c("cstat", "OE"), studlab = NULL,
                   method = "REML", level = 0.95, ...) {
  measure <- match.arg(measure)
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    pr_stop("`level` must be a single number between 0 and 1; got ",
            paste(format(level), collapse = ", "), ".")
  }

  tab  <- pr_prepare(data, measure, studlab)
  slab <- pr_slab(data, studlab)

  # valmeta takes the reported columns, not a pre-computed estimate: it does
  # its own reconciling internally and the argument names are the column
  # names, so the call is assembled from whichever ones `data` actually has.
  data <- pr_map(data, measure, quiet = TRUE)
  have <- intersect(PR_COLS[[measure]], names(data))
  args <- c(list(measure = measure, data = data, method = method, slab = slab),
            stats::setNames(lapply(have, as.name), have), list(...))
  fit  <- do.call(metamisc::valmeta, args)

  out <- pr_result(fit, tab, measure, method, level, data)
  out
}

#' Assemble what the caller actually reads
#'
#' valmeta returns the fit. A reader wants the pooled number, the interval
#' around it, the interval the next study should expect, and how much of the
#' spread is real. Those are pulled out here once so every print method, plot
#' and table in the package reads the same fields.
#'
#' @keywords internal
#' @noRd
pr_result <- function(fit, tab, measure, method, level, data) {
  est <- tryCatch(as.numeric(fit$est), error = function(e) NA_real_)
  cil <- tryCatch(as.numeric(fit$ci.lb), error = function(e) NA_real_)
  ciu <- tryCatch(as.numeric(fit$ci.ub), error = function(e) NA_real_)
  pil <- tryCatch(as.numeric(fit$pi.lb), error = function(e) NA_real_)
  piu <- tryCatch(as.numeric(fit$pi.ub), error = function(e) NA_real_)
  tau2 <- tryCatch(as.numeric(fit$fit$tau2), error = function(e) NA_real_)
  i2   <- tryCatch(as.numeric(fit$fit$I2), error = function(e) NA_real_)
  if (length(i2) == 0L || is.na(i2)) i2 <- pr_i2(tab$est, tab$se, tau2)

  structure(list(
    measure   = measure,
    label     = if (measure == "cstat") "c-statistic" else "O:E ratio",
    method    = method,
    level     = level,
    k         = nrow(tab),
    estimate  = est, ci.lb = cil, ci.ub = ciu,
    pi.lb     = pil, pi.ub = piu,
    tau2      = tau2, I2 = i2,
    table     = tab,
    fit       = fit,
    data      = data
  ), class = "easypr_result")
}

#' I-squared from the study variances, when the fit did not report one
#'
#' Higgins' definition, with the typical within-study variance taken as the
#' one that makes the weights add up the way they do in the fit.
#'
#' @keywords internal
#' @noRd
pr_i2 <- function(est, se, tau2) {
  if (!is.finite(tau2)) return(NA_real_)
  w  <- 1 / se^2
  k  <- length(est)
  if (k < 2L) return(NA_real_)
  s2 <- (k - 1) * sum(w) / (sum(w)^2 - sum(w^2))
  v  <- 100 * tau2 / (tau2 + s2)
  max(0, min(100, v))
}
