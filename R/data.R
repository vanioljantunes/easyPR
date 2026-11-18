#' External validations of eight covid-19 mortality models
#'
#' Every hospital or country in which de Jong and colleagues validated a
#' covid-19 mortality model, with the four performance measures they reported
#' for each one: the c-statistic, the calibration slope, the
#' calibration-in-the-large and the O:E ratio, each with its 95% confidence
#' interval. It is the rare published table that gives all four per
#' validation, which is what pooling the slope beside the c-statistic needs.
#'
#' The values are as printed in the supplement, including the few the source
#' got wrong. Those rows say what is wrong in `flags`, and the rows the
#' authors left out of their own meta-analysis say so in `excluded`. The Xie
#' model, nine validations with no flagged slope, reproduces the paper's
#' pooled results and is the one the book uses.
#'
#' @format A data frame with 78 rows, one per validation, and 20 columns:
#' \describe{
#'   \item{model}{The model validated, e.g. `"Xie"`, `"4C Mortality Score"`.}
#'   \item{Study}{Hospital, registry or consortium the validation used.}
#'   \item{country}{Country of that cluster.}
#'   \item{N, O}{Patients and deaths, where the paper gives them.}
#'   \item{cstat, cstat.cilb, cstat.ciub}{c-statistic and its 95% CI.}
#'   \item{slope, slope.cilb, slope.ciub}{Calibration slope and its 95% CI.}
#'   \item{citl, citl.cilb, citl.ciub}{Calibration-in-the-large and its 95% CI.}
#'   \item{OE, OE.cilb, OE.ciub}{Observed over expected deaths and its 95% CI.}
#'   \item{table}{Supplement table the row was read from.}
#'   \item{excluded}{Why the authors left the row out of a meta-analysis, if
#'     they did.}
#'   \item{flags}{Errors in the source, kept rather than corrected.}
#' }
#' @source de Jong, 2022; Clinical prediction models for mortality in patients
#'   with covid-19: external validation and individual participant data
#'   meta-analysis; 10.1136/bmj-2021-069881. Supplement Tables S2 to S9.
#' @examples
#' xie <- subset(dejong, model == "Xie")
#' easypr(xie, sm = "slope")
"dejong"
