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
pr_stop <- function(...) stop("easypr: ", ..., call. = FALSE)

#' Columns of `data` that a measure can be built from
#' @keywords internal
#' @noRd
PR_COLS <- list(
  cstat = c("cstat", "cstat.se", "cstat.cilb", "cstat.ciub", "N", "O", "E",
            "sd.LP"),
  OE    = c("OE", "OE.se", "OE.cilb", "OE.ciub", "N", "O", "E", "Po", "Pe",
            "citl", "citl.se")
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
  measure <- match.arg(measure, c("cstat", "OE"))
  data <- pr_map(data, measure)

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
  sd.LP      = c("sd.LP", "sdLP", "sd.lp")
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
