#' Pool a prognostic meta-analysis
#'
#' One call takes a table of studies and the summary measure they reported,
#' and returns the pooled result with its heterogeneity and a prediction
#' interval.
#'
#' `sm` is the only argument you must give thought to, and it is a question
#' about the review rather than about the spreadsheet. `"HR"`, `"OR"` and
#' `"RR"` pool what one **prognostic factor** does to risk. `"cstat"` pools a
#' **model's** discrimination: can it tell who is at higher risk from who is
#' at lower risk. `"OE"` pools that model's calibration-in-the-large: are the
#' risks it quotes the risks that actually happen. Discrimination and
#' calibration can point in different directions, and a review that reports
#' only the first is quietly leaving out whether the numbers mean anything.
#'
#' The prediction interval is the one to read. A confidence interval says
#' where the average sits; a prediction interval says what the next setting
#' should expect, which is the question a reader of a prognostic review
#' actually has.
#'
#' You will recognise the arguments if you have used **{meta}**: `sm`,
#' `studlab`, `method`, `method.tau`, `level`, `common` and `random` mean here
#' what they mean there. easyPR works out each study's estimate and standard
#' error first, from whichever quantities it reported, so the same call works
#' whether the papers gave confidence intervals, standard errors, or only
#' counts.
#'
#' @param data A data frame, one row per study or per validation.
#' @param sm The summary measure: `"HR"`, `"OR"` or `"RR"` for a prognostic
#'   factor, `"cstat"` for a model's discrimination, `"OE"` for its
#'   calibration.
#' @param studlab Column of `data` holding the study labels, or a vector of
#'   them. Left alone, easyPR looks for a `Study` column.
#' @param method Pooling method. `"Inverse"`, inverse-variance weighting, is
#'   the only one available for these measures, and anything else is refused
#'   by name rather than ignored.
#' @param method.tau Estimator for the between-study variance, `"REML"` by
#'   default.
#' @param method.ci How the interval around the pooled estimate is built.
#'   `"t"` is the Knapp-Hartung adjustment, which the prognosis handbook
#'   recommends and which matters most when there are few studies. `"z"` is
#'   the normal interval.
#' @param level Width of the confidence and prediction intervals.
#' @param common,random Which models to report. The random-effects model is
#'   the one easyPR prints; `common = TRUE` adds the fixed-effect estimate
#'   beside it.
#' @param ... Passed on to the estimator.
#'
#' @return An object of class `easypr_result`: a list with the pooled
#'   `estimate`, its interval, the prediction interval, `tau2`, `I2`, the
#'   per-study `table`, and the fit it came from.
#'
#' @examples
#' \donttest{
#' data(EuroSCORE, package = "metamisc")
#' easypr.object.cstat <- easypr(EuroSCORE, sm = "cstat")
#' easypr.object.cstat
#' }
#' @export
easypr <- function(data,
                   sm = "HR",
                   studlab = NULL,
                   method = "Inverse",
                   method.tau = "REML",
                   method.ci = c("t", "z"),
                   level = 0.95,
                   common = FALSE,
                   random = TRUE,
                   ...) {
  spec <- pr_sm(sm)
  pr_check_method(method)
  method.ci <- match.arg(method.ci)
  # metamisc applies Knapp-Hartung by default and metafor does not, so the
  # same call used to produce a t interval for a c-statistic and a z interval
  # for a hazard ratio. One argument now decides it for every measure.
  test <- if (identical(method.ci, "t")) "knha" else "z"
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    pr_stop("`level` must be a single number between 0 and 1; got ",
            paste(format(level), collapse = ", "), ".")
  }
  if (!isTRUE(random)) {
    pr_stop("`random = FALSE` is not available: a prognostic review is ",
            "summarised by a random-effects model and its prediction ",
            "interval. Use `common = TRUE` to see the fixed-effect estimate ",
            "beside it.")
  }

  tab  <- pr_prepare(data, spec$sm, studlab)
  slab <- pr_slab(data, studlab)

  if (spec$kind == "ratio") {
    fit <- metafor::rma(yi = tab$est, sei = tab$se, method = method.tau,
                        test = test, slab = tab$studlab, ...)
    return(pr_result_ratio(fit, tab, spec, method, method.tau, method.ci,
                           level, common, data))
  }

  # valmeta takes the reported columns, not a pre-computed estimate: it does
  # its own reconciling internally and the argument names are the column
  # names, so the call is assembled from whichever ones `data` actually has.
  data <- pr_map(data, spec, quiet = TRUE)
  have <- intersect(PR_COLS[[spec$kind]], names(data))
  args <- c(list(measure = spec$kind, data = data, method = method.tau,
                 test = test, slab = slab),
            stats::setNames(lapply(have, as.name), have), list(...))
  fit  <- do.call(metamisc::valmeta, args)

  pr_result(fit, tab, spec, method, method.tau, method.ci, level, common,
            data)
}

#' Only one pooling method exists for these measures
#'
#' {meta} offers Mantel-Haenszel, Peto and the rest because it pools raw
#' counts. Here the input is already an estimate and its standard error, so
#' inverse-variance weighting is the only thing that can be done with it. A
#' caller who names another one is told that, rather than having their
#' argument quietly ignored.
#'
#' @keywords internal
#' @noRd
pr_check_method <- function(method) {
  if (!is.character(method) || length(method) != 1L) {
    pr_stop("`method` must be \"Inverse\".")
  }
  if (!identical(tolower(method), "inverse")) {
    pr_stop("`method = \"", method, "\"` is not available. These measures ",
            "reach easyPR as an estimate and a standard error, so ",
            "inverse-variance weighting is the only pooling there is: use ",
            "`method = \"Inverse\"`. The between-study variance estimator is ",
            "`method.tau`, which does take several values.")
  }
  invisible(TRUE)
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
pr_result <- function(fit, tab, spec, method, method.tau, method.ci, level,
                      common, data) {
  # valmeta reports the summary on the scale the studies reported, and pools
  # it on another one. The table beside it is already on the pooling scale,
  # so the summary is moved onto it too and the whole object is then on one
  # scale, with the back-transform applied once when a number is printed.
  f   <- pr_fwd(spec$scale)
  num <- function(v) tryCatch(f(as.numeric(v)), error = function(e) NA_real_)
  est <- num(fit$est)
  cil <- num(fit$ci.lb)
  ciu <- num(fit$ci.ub)
  pil <- num(fit$pi.lb)
  piu <- num(fit$pi.ub)
  tau2 <- tryCatch(as.numeric(fit$fit$tau2), error = function(e) NA_real_)
  i2   <- tryCatch(as.numeric(fit$fit$I2), error = function(e) NA_real_)
  if (length(i2) == 0L || is.na(i2)) i2 <- pr_i2(tab$est, tab$se, tau2)

  structure(c(pr_common_fields(spec, method, method.tau, method.ci, level,
                               common, tab),
              list(estimate = est, ci.lb = cil, ci.ub = ciu,
                   pi.lb = pil, pi.ub = piu,
                   tau2 = tau2, I2 = i2,
                   test = pr_test(fit$fit),
                   fixed = pr_fixed(tab),
                   fit = fit, data = data)),
            class = "easypr_result")
}

#' The test of the pooled effect, whichever object carries it
#'
#' valmeta keeps the metafor fit in `$fit`, and metafor names the statistic
#' `zval` whether it is a z or a t. The interval method says which it is.
#'
#' @keywords internal
#' @noRd
pr_test <- function(f) {
  num <- function(v) tryCatch(as.numeric(v)[1], error = function(e) NA_real_)
  list(stat = num(f$zval), p = num(f$pval),
       name = if (identical(f$test, "z")) "z" else "t",
       df = num(f$ddf))
}

#' The fields every result carries, whichever estimator produced it
#' @keywords internal
#' @noRd
pr_common_fields <- function(spec, method, method.tau, method.ci, level,
                             common, tab) {
  list(sm         = spec$sm,
       measure    = spec$kind,
       label      = spec$label,
       unit       = spec$unit,
       scale      = spec$scale,
       null       = spec$null,
       what       = spec$what,
       columns    = spec$cols,
       log        = identical(spec$scale, "log"),
       method     = method,
       method.tau = method.tau,
       method.ci  = method.ci,
       level      = level,
       common     = isTRUE(common),
       k          = nrow(tab),
       table      = tab)
}

#' The fixed-effect estimate, for a caller who asked to see it beside
#' @keywords internal
#' @noRd
pr_fixed <- function(tab) {
  w  <- 1 / tab$se^2
  mu <- sum(w * tab$est) / sum(w)
  se <- sqrt(1 / sum(w))
  z  <- stats::qnorm(0.975)
  list(estimate = mu, ci.lb = mu - z * se, ci.ub = mu + z * se)
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

#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

#' The ratio measures' result
#'
#' Everything downstream works on the log scale, because that is where the
#' pooling happens and where a symmetric interval belongs. Only the printed
#' numbers are exponentiated, and the `log` flag tells the plots to label a
#' ratio axis rather than a difference one.
#'
#' @keywords internal
#' @noRd
pr_result_ratio <- function(fit, tab, spec, method, method.tau, method.ci,
                            level, common, data) {
  z   <- stats::qnorm(1 - (1 - level) / 2)
  mu  <- as.numeric(fit$b)
  pse <- sqrt(as.numeric(fit$tau2) + as.numeric(fit$se)^2)
  structure(c(pr_common_fields(spec, method, method.tau, method.ci, level,
                               common, tab),
              list(estimate = mu, ci.lb = fit$ci.lb, ci.ub = fit$ci.ub,
                   pi.lb = mu - z * pse, pi.ub = mu + z * pse,
                   tau2 = as.numeric(fit$tau2), I2 = as.numeric(fit$I2),
                   test = pr_test(fit),
                   fixed = pr_fixed(tab),
                   fit = fit, data = data)),
            class = "easypr_result")
}
