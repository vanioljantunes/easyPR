# easyPR 0.0.0.9001

* `easypr()` pools two more calibration measures: `sm = "slope"` (calibration
  slope) and `sm = "citl"` (calibration-in-the-large). Both are pooled as
  reported, with the standard error taken from a `.se` column or from the
  95% confidence interval. With the Knapp-Hartung interval their prediction
  interval uses a t with k - 2 degrees of freedom, as `valmeta()` does for
  the c-statistic.
* Printing any calibration result reminds the reader to report the slope
  together with the O:E ratio or the calibration-in-the-large.
* New data `dejong`: 78 external validations of eight covid-19 mortality
  models with c-statistic, calibration slope, calibration-in-the-large and
  O:E ratio per validation (de Jong, 2022; 10.1136/bmj-2021-069881).
* First testthat suite.
