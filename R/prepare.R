# Getting one estimate and one standard error per validation.
#
# Validation studies report performance in whatever form their journal asked
# for. One gives the c-statistic with a confidence interval, the next gives it
# with a standard error, a third gives neither and leaves you the sample size
# and the number of events to work back from. The same is true of calibration:
# sometimes the O:E ratio, sometimes the observed and expected counts, and
# sometimes only the proportions.
#
# metamisc::ccalc() and metamisc::oecalc() already know how to reconcile
# those, so easyPR's job is to hand them the columns they want and to fail
# with a sentence a reader can act on when the table cannot support the
# measure that was asked for.

#' @keywords internal
#' @noRd
pr_stop <- function(...) {
  # An error message is read on a console, and sometimes photographed for a
  # slide, so it is wrapped at the width the rest of the output uses rather
  # than running off the right of the window as one line.
  msg <- paste0("easypr: ", paste0(..., collapse = ""))
  stop(paste(strwrap(msg, width = 72, exdent = 2), collapse = "
"),
       call. = FALSE)
}

#' Columns of `data` that a measure can be built from
#' @keywords internal
#' @noRd
PR_COLS <- list(
  cstat = c("cstat", "cstat.se", "cstat.cilb", "cstat.ciub", "N", "O", "E",
            "sd.LP"),
  OE    = c("OE", "OE.se", "OE.cilb", "OE.ciub", "N", "O", "E", "Po", "Pe",
            "citl", "citl.se"),
  ratio = c("effect", "effect.se", "effect.cilb", "effect.ciub")
)

#' Build the per-study table for one measure
#'
#' The returned frame always has `studlab`, `est` and `se`, whatever the input
#' reported, so every plot downstream can be written against those three.
#'
#' @keywords internal
#' @noRd
pr_prepare <- function(data, measure, studlab = NULL) {
  if (!is.data.frame(data)) pr_stop("`data` must be a data frame of validations.")
  measure <- match.arg(measure, c("cstat", "OE", "ratio"))
  data <- pr_map(data, measure)
  if (measure == "ratio") return(pr_prepare_ratio(data, studlab))

  have <- intersect(PR_COLS[[measure]], names(data))
  if (length(have) == 0L) {
    pr_stop("none of the columns ", measure, " can be built from are in `data`. ",
            "For measure = \"", measure, "\" easyPR looks for: ",
            paste(PR_COLS[[measure]], collapse = ", "), ".")
  }

  slab <- pr_slab(data, studlab)
  args <- c(list(data = data, slab = slab), stats::setNames(
    lapply(have, function(cl) as.name(cl)), have))

  calc <- if (measure == "cstat") metamisc::ccalc else metamisc::oecalc
  out  <- do.call(calc, args)

  tab <- data.frame(
    studlab = as.character(slab),
    est     = as.numeric(out$theta),
    se      = as.numeric(out$theta.se),
    lower   = as.numeric(out$theta.cilb),
    upper   = as.numeric(out$theta.ciub),
    stringsAsFactors = FALSE
  )
  usable <- is.finite(tab$est) & is.finite(tab$se) & tab$se > 0
  if (!any(usable)) {
    pr_stop("no validation in `data` ended up with both an estimate and a ",
            "standard error for ", measure, ". Check that the columns hold ",
            "numbers rather than text.")
  }
  if (any(!usable)) {
    message("easypr: ", sum(!usable), " of ", nrow(tab),
            " validations could not be used, because ", measure,
            " could not be reconstructed from what they reported.")
  }
  attr(tab, "dropped") <- tab$studlab[!usable]
  tab[usable, , drop = FALSE]
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

#' Rename what the table has to what valmeta expects
#'
#' Returns the data frame with canonical names added, and reports the mapping
#' once so the reader can see what easyPR decided their columns meant.
#'
#' @keywords internal
#' @noRd
pr_map <- function(data, measure, quiet = FALSE) {
  wanted <- PR_COLS[[measure]]
  found  <- character(0)
  for (canon in wanted) {
    if (canon %in% names(data)) { found[canon] <- canon; next }
    hit <- intersect(PR_ALIAS[[canon]], names(data))
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
pr_prepare_ratio <- function(data, studlab) {
  slab <- pr_slab(data, studlab)
  # the column it was read from names the measure, so a hazard ratio is
  # labelled a hazard ratio rather than "ratio"
  src  <- attr(data, "pr_found")[["effect"]]
  nm   <- c(HR = "hazard ratio", hr = "hazard ratio", OR = "odds ratio",
            or = "odds ratio", RR = "risk ratio", rr = "risk ratio")[src]
  if (is.na(nm) || is.null(nm)) nm <- "ratio"
  eff  <- as.numeric(data[["effect"]])
  if (all(is.na(eff))) {
    pr_stop("measure = \"ratio\" needs an `effect` column holding the hazard, ",
            "odds or risk ratio. Recognised spellings: ",
            paste(PR_ALIAS$effect, collapse = ", "), ".")
  }
  if (any(eff <= 0, na.rm = TRUE)) {
    pr_stop("a ratio cannot be zero or negative, and ", sum(eff <= 0, na.rm = TRUE),
            " row(s) are. Give the ratio itself, not its logarithm.")
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
            "interval for the ratio, so there is nothing to weight them by. ",
            "Recognised interval columns: ",
            paste(c(PR_ALIAS$effect.cilb, PR_ALIAS$effect.ciub),
                  collapse = ", "), ".")
  }

  tab <- data.frame(studlab = as.character(slab), est = yi, se = se,
                    lower = log(lo), upper = log(hi),
                    stringsAsFactors = FALSE)
  tab$lower[is.na(tab$lower)] <- tab$est[is.na(tab$lower)] - 1.96 * tab$se[is.na(tab$lower)]
  tab$upper[is.na(tab$upper)] <- tab$est[is.na(tab$upper)] + 1.96 * tab$se[is.na(tab$upper)]
  usable <- is.finite(tab$est) & is.finite(tab$se) & tab$se > 0
  if (!any(usable)) {
    pr_stop("no study has both a ratio and something to get its standard ",
            "error from.")
  }
  attr(tab, "effect_label") <- unname(nm)
  tab[usable, , drop = FALSE]
}
