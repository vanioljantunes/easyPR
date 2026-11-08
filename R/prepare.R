# Getting one estimate and one standard error per study.
#
# Prognostic studies report what they report. One gives the c-statistic with a
# confidence interval, the next gives it with a standard error, a third gives
# neither and leaves you the sample size and the number of events to work back
# from. The same is true of calibration, and of a hazard ratio.
#
# metamisc::ccalc() and metamisc::oecalc() already know how to reconcile
# those, so easyPR's job is to hand them the columns they want and to fail
# with a sentence a reader can act on when the table cannot support the
# summary measure that was asked for.

#' @keywords internal
#' @noRd
pr_stop <- function(...) {
  # An error message is read on a console, and sometimes photographed for a
  # slide, so it is wrapped at the width the rest of the output uses rather
  # than running off the right of the window as one line.
  msg <- paste0("easypr: ", paste0(..., collapse = ""))
  stop(paste(strwrap(msg, width = 72, exdent = 2), collapse = "\n"),
       call. = FALSE)
}

# The summary measure decides everything downstream.
#
# `sm` is the {meta} spelling, and it means here what it means there: the
# measure the studies reported. It picks which columns are read, which scale
# the pooling happens on, what the axis of a forest says, and which columns
# that forest carries beside the effect. Nothing else in easyPR asks the
# caller to choose any of those separately.

#' @keywords internal
#' @noRd
PR_SM <- list(
  HR    = list(kind = "ratio", label = "hazard ratio", unit = "HR",
               scale = "log",   null = 1,
               what = "study", cols = c(N = "N")),
  OR    = list(kind = "ratio", label = "odds ratio", unit = "OR",
               scale = "log",   null = 1,
               what = "study", cols = c(N = "N")),
  RR    = list(kind = "ratio", label = "risk ratio", unit = "RR",
               scale = "log",   null = 1,
               what = "study", cols = c(N = "N")),
  cstat = list(kind = "cstat", label = "c-statistic", unit = "c",
               scale = "logit", null = NA_real_,
               what = "validation", cols = c(N = "N", Events = "O")),
  OE    = list(kind = "OE",    label = "O:E ratio", unit = "O:E",
               scale = "log",   null = 1,
               what = "validation", cols = c(Observed = "O", Expected = "E"))
)

#' The summary measures easypr() accepts
#' @keywords internal
#' @noRd
PR_SM_NAMES <- names(PR_SM)

#' Look one up, and refuse an unknown one by name
#' @keywords internal
#' @noRd
pr_sm <- function(sm) {
  if (!is.character(sm) || length(sm) != 1L) {
    pr_stop("`sm` must be one of ", paste(PR_SM_NAMES, collapse = ", "), ".")
  }
  hit <- PR_SM_NAMES[tolower(PR_SM_NAMES) == tolower(sm)]
  if (length(hit) == 0L) {
    pr_stop("`sm = \"", sm, "\"` is not a measure easyPR pools. Use one of: ",
            paste(PR_SM_NAMES, collapse = ", "), ". \"HR\", \"OR\" and ",
            "\"RR\" pool a prognostic factor; \"cstat\" and \"OE\" pool a ",
            "model's discrimination and its calibration.")
  }
  c(PR_SM[[hit]], list(sm = hit))
}

# One scale inside, the reader's scale outside.
#
# metamisc hands back two different scales at once: ccalc() and oecalc() give
# the per-study estimate as the study reported it, while valmeta() pools the
# c-statistic on the logit scale and the O:E ratio on the log scale and
# reports the summary back on the reported one. Keeping both inside easyPR is
# what let tau-squared be exponentiated with the effect once already.
#
# So everything inside the result object is on the scale the pooling happened
# on, and the back-transform is applied once, at the moment a number is shown.

#' @keywords internal
#' @noRd
pr_fwd <- function(scale) {
  switch(scale, log = log, logit = stats::qlogis, identity)
}

#' @keywords internal
#' @noRd
pr_bwd <- function(scale) {
  switch(scale, log = exp, logit = stats::plogis, identity)
}

#' The plural of whatever `sm` calls one row
#'
#' A prognostic factor is pooled across studies and a model across
#' validations, and "20 studys" is the kind of thing a reader notices before
#' they notice the estimate.
#'
#' @keywords internal
#' @noRd
pr_plural <- function(what, n = 2L) {
  if (n == 1L) return(what)
  if (identical(what, "study")) return("studies")
  paste0(what, "s")
}

#' Columns of `data` that a measure can be built from
#' @keywords internal
#' @noRd
PR_COLS <- list(
  cstat = c("cstat", "cstat.se", "cstat.cilb", "cstat.ciub", "N", "O", "E",
            "sd.LP"),
  OE    = c("OE", "OE.se", "OE.cilb", "OE.ciub", "N", "O", "E", "Po", "Pe",
            "citl", "citl.se"),
  ratio = c("effect", "effect.se", "effect.cilb", "effect.ciub", "N", "O")
)

#' Build the per-study table for one summary measure
#'
#' The returned frame always has `studlab`, `est` and `se`, whatever the input
#' reported, so every plot downstream can be written against those three. The
#' columns the forest shows beside the effect ride along with it.
#'
#' @keywords internal
#' @noRd
pr_prepare <- function(data, sm, studlab = NULL) {
  if (!is.data.frame(data)) pr_stop("`data` must be a data frame of studies.")
  spec <- pr_sm(sm)
  data <- pr_map(data, spec)
  if (spec$kind == "ratio") return(pr_prepare_ratio(data, spec, studlab))

  have <- intersect(PR_COLS[[spec$kind]], names(data))
  if (length(have) == 0L) {
    pr_stop("none of the columns ", spec$sm, " can be built from are in ",
            "`data`. For sm = \"", spec$sm, "\" easyPR looks for: ",
            paste(PR_COLS[[spec$kind]], collapse = ", "), ".")
  }

  slab <- pr_slab(data, studlab)
  args <- c(list(data = data, slab = slab), stats::setNames(
    lapply(have, function(cl) as.name(cl)), have))

  calc <- if (spec$kind == "cstat") metamisc::ccalc else metamisc::oecalc
  out  <- do.call(calc, args)

  tab <- data.frame(
    studlab = as.character(slab),
    est     = as.numeric(out$theta),
    se      = as.numeric(out$theta.se),
    lower   = as.numeric(out$theta.cilb),
    upper   = as.numeric(out$theta.ciub),
    stringsAsFactors = FALSE
  )
  # ccalc() and oecalc() report on the scale the study reported. The pooling
  # happens on another one, so the table is moved onto it here and every
  # number in the package from this point on is on the pooling scale.
  tab <- pr_to_scale(tab, spec$scale)
  tab <- pr_side_columns(tab, data, spec)
  usable <- is.finite(tab$est) & is.finite(tab$se) & tab$se > 0
  if (!any(usable)) {
    pr_stop("no ", spec$what, " in `data` ended up with both an estimate and ",
            "a standard error for ", spec$sm, ". Check that the columns hold ",
            "numbers rather than text.")
  }
  if (any(!usable)) {
    message("easypr: ", sum(!usable), " of ", nrow(tab), " ",
            pr_plural(spec$what), " could not be used, because ", spec$sm,
            " could not be reconstructed from what they reported.")
  }
  attr(tab, "dropped") <- tab$studlab[!usable]
  tab[usable, , drop = FALSE]
}

#' Move a per-study table onto the scale the pooling happens on
#'
#' The standard error travels with it, by the delta method: a c-statistic of
#' 0.80 with a standard error of 0.02 is a logit of 1.386 with a standard
#' error of 0.125, and weighting the studies on the wrong one of those two
#' gives the wrong pooled answer rather than a differently labelled one.
#'
#' @keywords internal
#' @noRd
pr_to_scale <- function(tab, scale) {
  if (identical(scale, "identity")) return(tab)
  f <- pr_fwd(scale)
  # d(logit p)/dp = 1 / (p (1 - p));  d(log x)/dx = 1 / x
  slope <- switch(scale,
                  logit = 1 / (tab$est * (1 - tab$est)),
                  log   = 1 / tab$est,
                  rep(1, nrow(tab)))
  tab$se    <- tab$se * slope
  tab$est   <- f(tab$est)
  tab$lower <- f(tab$lower)
  tab$upper <- f(tab$upper)
  tab
}

#' The columns the forest shows beside the effect
#'
#' `sm` decides which ones those are, and a table that does not carry them is
#' not an error: the forest simply drops the column rather than printing a
#' row of NA.
#'
#' @keywords internal
#' @noRd
pr_side_columns <- function(tab, data, spec) {
  for (nm in names(spec$cols)) {
    src <- spec$cols[[nm]]
    v <- if (src %in% names(data)) data[[src]] else NULL
    tab[[nm]] <- if (is.null(v)) rep(NA_real_, nrow(tab)) else as.numeric(v)
  }
  tab
}

#' Study labels, from the column named or from the row names
#' @keywords internal
#' @noRd
pr_slab <- function(data, studlab) {
  if (is.null(studlab)) {
    for (cand in c("Study", "study", "studlab", "author", "Author")) {
      if (cand %in% names(data)) return(as.character(data[[cand]]))
    }
    return(paste("Study", seq_len(nrow(data))))
  }
  if (length(studlab) == 1L && studlab %in% names(data)) {
    return(as.character(data[[studlab]]))
  }
  if (length(studlab) != nrow(data)) {
    pr_stop("`studlab` must name a column of `data` or be one label per row; ",
            "got ", length(studlab), " labels for ", nrow(data), " rows.")
  }
  as.character(studlab)
}

# Column aliases.
#
# valmeta names its arguments cstat, cstat.se, N, O, E. Almost nobody names
# their columns that. metamisc's own EuroSCORE data calls the c-statistic
# c.index, its standard error se.c.index and the events n.events, and a
# spreadsheet from a review will have its own third set of names. Rather than
# make the reader rename columns before they can fit anything, easyPR
# recognises the common spellings and says which ones it matched.

#' @keywords internal
#' @noRd
PR_ALIAS <- list(
  cstat      = c("cstat", "c.index", "c_index", "cindex", "c.statistic",
                 "cstatistic", "auc", "AUC", "C", "c"),
  cstat.se   = c("cstat.se", "se.c.index", "se.cstat", "se.cindex", "se.auc",
                 "c.index.se", "seAUC"),
  cstat.cilb = c("cstat.cilb", "c.index.95CIl", "c.index.cilb", "auc.cilb",
                 "ci.lb", "cstat.lower"),
  cstat.ciub = c("cstat.ciub", "c.index.95CIu", "c.index.ciub", "auc.ciub",
                 "ci.ub", "cstat.upper"),
  OE         = c("OE", "oe", "OE.ratio", "oe.ratio"),
  OE.se      = c("OE.se", "oe.se", "se.OE"),
  OE.cilb    = c("OE.cilb", "oe.cilb", "OE.95CIl"),
  OE.ciub    = c("OE.ciub", "oe.ciub", "OE.95CIu"),
  N          = c("N", "n", "n.total", "sample.size"),
  O          = c("O", "n.events", "o.events", "observed", "events"),
  E          = c("E", "e.events", "expected"),
  Po         = c("Po", "po", "prop.observed"),
  Pe         = c("Pe", "pe", "prop.expected"),
  citl       = c("citl", "CITL"),
  citl.se    = c("citl.se", "se.citl"),
  sd.LP      = c("sd.LP", "sdLP", "sd.lp"),
  effect     = c("effect", "HR", "hr", "OR", "or", "RR", "rr", "ratio",
                 "estimate", "TE"),
  effect.se  = c("effect.se", "se", "seTE", "se.HR", "se.log", "selogHR"),
  effect.cilb = c("effect.cilb", "lower", "ci.lb", "lower95", "HR.cilb",
                  "HR.025", "OR.025", "RR.025", "lcl", "LCL", "lower.CI"),
  effect.ciub = c("effect.ciub", "upper", "ci.ub", "upper95", "HR.ciub",
                  "HR.975", "OR.975", "RR.975", "ucl", "UCL", "upper.CI")
)

#' The aliases to try, with the ones named after `sm` first
#'
#' A sheet that holds both an odds ratio and a hazard ratio is ordinary in a
#' prognostic review. Asking for `sm = "HR"` should read the hazard ratio,
#' not whichever column happens to come first in the alias list, so the
#' measure the caller named is tried before anything else.
#'
#' @keywords internal
#' @noRd
pr_alias_for <- function(canon, spec) {
  base <- PR_ALIAS[[canon]]
  if (spec$kind != "ratio") return(base)
  sm <- spec$sm
  first <- switch(canon,
                  effect      = c(sm, tolower(sm)),
                  effect.se   = paste0(c("se.", "se"), sm),
                  effect.cilb = paste0(sm, c(".025", ".cilb", ".lower")),
                  effect.ciub = paste0(sm, c(".975", ".ciub", ".upper")),
                  character(0))
  unique(c(first, base))
}

#' Rename what the table has to what valmeta expects
#'
#' Returns the data frame with canonical names added, and reports the mapping
#' once so the reader can see what easyPR decided their columns meant.
#'
#' @keywords internal
#' @noRd
pr_map <- function(data, spec, quiet = FALSE) {
  wanted <- PR_COLS[[spec$kind]]
  found  <- character(0)
  for (canon in wanted) {
    if (canon %in% names(data)) { found[canon] <- canon; next }
    hit <- intersect(pr_alias_for(canon, spec), names(data))
    if (length(hit) > 0L) {
      data[[canon]] <- data[[hit[1L]]]
      found[canon]  <- hit[1L]
    }
  }
  renamed <- found[names(found) != unname(found)]
  if (!quiet && length(renamed) > 0L) {
    message("easypr: read ",
            paste(sprintf("%s as %s", unname(renamed), names(renamed)),
                  collapse = ", "), ".")
  }
  attr(data, "pr_found") <- found
  data
}


# A prognostic factor rather than a model.
#
# The other half of a prognostic review pools what one factor does to risk:
# a hazard ratio, or an odds or risk ratio. Those are pooled on the log
# scale and reported back on the ratio scale, which is the step most often
# got wrong when it is done by hand, so easyPR does it and says so.

#' @keywords internal
#' @noRd
pr_prepare_ratio <- function(data, spec, studlab) {
  slab <- pr_slab(data, studlab)
  eff  <- as.numeric(data[["effect"]])
  if (length(eff) == 0L || all(is.na(eff))) {
    pr_stop("sm = \"", spec$sm, "\" needs a column holding the ", spec$label,
            " itself. Recognised spellings: ",
            paste(pr_alias_for("effect", spec), collapse = ", "), ".")
  }
  if (any(eff <= 0, na.rm = TRUE)) {
    pr_stop("a ", spec$label, " cannot be zero or negative, and ",
            sum(eff <= 0, na.rm = TRUE), " row(s) are. Give the ratio ",
            "itself, not its logarithm.")
  }
  yi <- log(eff)
  col <- function(nm) {
    v <- data[[nm]]
    if (is.null(v) || length(v) == 0L) rep(NA_real_, nrow(data)) else as.numeric(v)
  }
  se <- col("effect.se")
  lo <- col("effect.cilb")
  hi <- col("effect.ciub")
  # a confidence interval on the ratio scale implies the standard error on
  # the log scale, which is how most papers leave it
  from_ci <- (log(hi) - log(lo)) / (2 * stats::qnorm(0.975))
  se[is.na(se)] <- from_ci[is.na(se)]
  if (all(is.na(se))) {
    pr_stop("every study is missing both a standard error and a confidence ",
            "interval for the ", spec$label, ", so there is nothing to ",
            "weight them by. Recognised interval columns: ",
            paste(c(pr_alias_for("effect.cilb", spec),
                    pr_alias_for("effect.ciub", spec)), collapse = ", "), ".")
  }

  tab <- data.frame(studlab = as.character(slab), est = yi, se = se,
                    lower = log(lo), upper = log(hi),
                    stringsAsFactors = FALSE)
  tab$lower[is.na(tab$lower)] <- tab$est[is.na(tab$lower)] - 1.96 * tab$se[is.na(tab$lower)]
  tab$upper[is.na(tab$upper)] <- tab$est[is.na(tab$upper)] + 1.96 * tab$se[is.na(tab$upper)]
  tab <- pr_side_columns(tab, data, spec)
  usable <- is.finite(tab$est) & is.finite(tab$se) & tab$se > 0
  if (!any(usable)) {
    pr_stop("no study has both a ", spec$label, " and something to get its ",
            "standard error from.")
  }
  tab[usable, , drop = FALSE]
}
