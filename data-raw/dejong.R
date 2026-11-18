# Builds data/dejong.rda from data-raw/dejong2022.csv.
#
# The CSV was typed from the supplement (Tables S2 to S9) of de Jong, 2022;
# Clinical prediction models for mortality in patients with covid-19: external
# validation and individual participant data meta-analysis;
# 10.1136/bmj-2021-069881. Values are kept as printed, including the few the
# source got wrong; those rows say so in `flags`.
#
# Run from the package root:
#   Rscript data-raw/dejong.R

raw <- utils::read.csv("data-raw/dejong2022.csv", stringsAsFactors = FALSE)

dejong <- data.frame(
  model      = raw$model,
  Study      = raw$cluster,
  country    = raw$country,
  N          = raw$n,
  O          = raw$deaths,
  cstat      = raw$c,
  cstat.cilb = raw$c_lb,
  cstat.ciub = raw$c_ub,
  slope      = raw$slope,
  slope.cilb = raw$slope_lb,
  slope.ciub = raw$slope_ub,
  citl       = raw$citl,
  citl.cilb  = raw$citl_lb,
  citl.ciub  = raw$citl_ub,
  OE         = raw$oe,
  OE.cilb    = raw$oe_lb,
  OE.ciub    = raw$oe_ub,
  table      = raw$source_table,
  excluded   = raw$ma_exclusion,
  flags      = raw$flags,
  stringsAsFactors = FALSE
)

save(dejong, file = "data/dejong.rda", compress = "xz")
