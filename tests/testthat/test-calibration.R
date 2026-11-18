# The two calibration measures added in 0.0.0.9001, checked against the
# numbers de Jong 2022 printed for the Xie model and against a fit done by
# hand with metafor.

xie <- subset(dejong, model == "Xie")

test_that("slope reproduces de Jong 2022 for the Xie model", {
  r <- easypr(xie, sm = "slope")
  expect_s3_class(r, "easypr_result")
  expect_equal(r$k, 9L)
  expect_equal(round(r$estimate, 2), 0.45)
  expect_equal(round(c(r$ci.lb, r$ci.ub), 2), c(0.27, 0.63))
  expect_identical(r$scale, "identity")
  expect_identical(r$null, 1)
})

test_that("citl reproduces de Jong 2022 for the Xie model", {
  r <- easypr(xie, sm = "citl")
  expect_equal(round(r$estimate, 2), -0.10)
  expect_equal(round(c(r$ci.lb, r$ci.ub), 2), c(-1.00, 0.81))
  expect_identical(r$null, 0)
})

test_that("slope matches a hand fit and a t(k - 2) prediction interval", {
  r  <- easypr(xie, sm = "slope")
  se <- (xie$slope.ciub - xie$slope.cilb) / (2 * qnorm(0.975))
  f  <- metafor::rma(yi = xie$slope, sei = se, method = "REML", test = "knha")
  expect_equal(r$estimate, as.numeric(f$b), tolerance = 1e-8)
  expect_equal(r$tau2, f$tau2, tolerance = 1e-8)
  half <- qt(0.975, df = nrow(xie) - 2) * sqrt(f$tau2 + f$se^2)
  expect_equal(c(r$pi.lb, r$pi.ub), as.numeric(f$b) + c(-1, 1) * half,
               tolerance = 1e-8)
})

test_that("a z interval keeps a normal prediction interval", {
  r <- easypr(xie, sm = "slope", method.ci = "z")
  f <- metafor::rma(yi = r$table$est, sei = r$table$se, method = "REML")
  half <- qnorm(0.975) * sqrt(f$tau2 + f$se^2)
  expect_equal(r$pi.ub - r$pi.lb, 2 * half, tolerance = 1e-8)
})

test_that("a standard error column is used before the interval", {
  d <- data.frame(Study = paste("S", 1:4), slope = c(0.8, 1.1, 0.9, 1.0),
                  slope.se = c(0.10, 0.20, 0.15, 0.12))
  r <- easypr(d, sm = "slope")
  expect_equal(r$table$se, d$slope.se)
})

test_that("common column spellings are recognised", {
  d <- data.frame(Study = paste("S", 1:3), CITL = c(-0.2, 0.1, 0.3),
                  citl_lb = c(-0.5, -0.2, 0.0), citl_ub = c(0.1, 0.4, 0.6))
  expect_message(r <- easypr(d, sm = "citl"), "citl_lb as citl.cilb")
  expect_equal(r$k, 3L)
})

test_that("a missing measure or interval is refused by name", {
  expect_error(easypr(data.frame(x = 1:3), sm = "slope"),
               "calibration slope")
  expect_error(easypr(data.frame(slope = c(1, 0.9, 1.1)), sm = "slope"),
               "standard error")
})

test_that("rows without an interval are dropped with a message", {
  d <- data.frame(Study = paste("S", 1:4), slope = c(0.8, 1.1, 0.9, 1.0),
                  slope.se = c(0.10, NA, 0.15, 0.12))
  expect_message(r <- easypr(d, sm = "slope"), "1 of 4 validations")
  expect_equal(r$k, 3L)
})

test_that("the printout asks for both calibration measures", {
  out <- capture.output(print(easypr(xie, sm = "slope")))
  expect_true(any(grepl("half the picture", out)))
})

test_that("the forest and leave-one-out run for the new measures", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(capture.output(easypr_forest(easypr(xie, sm = "slope"))))
  expect_no_error(capture.output(easypr_loo(easypr(xie, sm = "citl"))))
})
