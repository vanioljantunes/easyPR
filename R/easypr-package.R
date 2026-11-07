#' easyPR: prognostic meta-analysis, made teachable
#'
#' A prognostic meta-analysis does not pool a treatment effect. It pools how
#' well a model performs when somebody else runs it on their own patients,
#' and performance has two halves that can disagree. Discrimination, the
#' c-statistic, asks whether the model ranks patients correctly. Calibration,
#' the observed-to-expected ratio, asks whether the risks it quotes are the
#' risks that happen. A model can rank well and still be badly calibrated,
#' which is why both are reported and why `easypr()` makes you name which one
#' you are pooling.
#'
#' Estimation is delegated to \pkg{metamisc}. What easyPR adds is the part
#' that is tedious to get right: working out each study's estimate and
#' standard error from whichever of the many reportable quantities it
#' happened to publish, and then laying the result out the way a published
#' review lays it out.
#'
#' @keywords internal
"_PACKAGE"
