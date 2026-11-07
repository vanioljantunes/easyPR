#' Meta-regression on a study-level covariate
#'
#' Heterogeneity in a prognostic review is usually the interesting part
#' rather than a nuisance: the model is being validated in different places
#' on different patients, and the question is whether that explains the
#' spread. A bubble plot is the honest way to show it, because the reader can
#' see how few studies are carrying each end of the line.
#'
#' The covariate is a property of the study, so this is an observational
#' comparison across studies and not a randomised one. A slope found here
#' says the studies differ, not that moving a patient along the covariate
#' would move their risk.
#'
#' @inheritParams easypr_forest
#' @param moderator Name of a column in the data the fit was built from.
#' @param ylab Y-axis label; the measure's name by default.
#' @return The ggplot, invisibly, with the `metafor` fit attached as the
#'   `"fit"` attribute.
#' @examples
#' \donttest{
#' data(EuroSCORE, package = "metamisc")
#' easypr_metareg(easypr(EuroSCORE, measure = "cstat"), "mean.age")
#' }
#' @export
easypr_metareg <- function(x, moderator, main = NULL, xlab = NULL, ylab = NULL,
                           result = TRUE,
                           filetype = c("png", "pdf", "tiff"), path = NULL,
                           file.name = NULL, width = NULL, height = NULL) {
  pr_check(x, "easypr_metareg()")
  if (missing(moderator) || length(moderator) != 1L || !is.character(moderator)) {
    pr_stop("`moderator` must be the name of one column of the data the fit ",
            "was built from.")
  }
  d <- x$data
  if (!moderator %in% names(d)) {
    pr_stop("`", moderator, "` is not a column of the data. Available: ",
            paste(names(d), collapse = ", "), ".")
  }

  t <- x$table
  # the fit dropped any validation it could not use, so the covariate is
  # matched back by study label rather than by row position
  slab <- pr_slab(d, NULL)
  mod  <- d[[moderator]][match(t$studlab, slab)]
  ok   <- is.finite(as.numeric(mod))
  if (sum(ok) < 3L) {
    pr_stop("only ", sum(ok), " validations have a value for `", moderator,
            "`, which is too few to regress on.")
  }
  if (any(!ok)) {
    message("easypr: ", sum(!ok), " validations have no value for `",
            moderator, "` and are left out of the regression.")
  }
  t <- t[ok, ]; t$mod <- as.numeric(mod[ok])
  t$w <- pr_weights(t, x$tau2)

  # The moderator is fitted from a frame of its own, under the name the
  # reader gave it: a formula written against t$mod prints the row of the
  # model table as "t$mod", which tells a class nothing.
  reg <- data.frame(yi = t$est, sei = t$se, mod = t$mod)
  names(reg)[3] <- moderator
  fit <- metafor::rma(yi = yi, sei = sei,
                      mods = stats::reformulate(moderator),
                      data = reg, method = "REML")
  line <- pr_metareg_line(fit, t$mod)

  if (is.null(xlab)) xlab <- moderator
  if (is.null(ylab)) ylab <- x$label
  if (is.null(main)) main <- paste0(x$label, " against ", moderator)

  p <- ggplot2::ggplot(t, ggplot2::aes(x = mod, y = est)) +
    ggplot2::geom_ribbon(data = line, ggplot2::aes(x = mod, ymin = lo, ymax = hi),
                         inherit.aes = FALSE, alpha = 0.15, fill = "#1b3a5c") +
    ggplot2::geom_line(data = line, ggplot2::aes(x = mod, y = fit),
                       inherit.aes = FALSE, colour = "#1b3a5c", linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(size = w), shape = 21, fill = "white",
                        colour = "#1b3a5c", stroke = 0.9) +
    ggplot2::scale_size_continuous(range = c(2, 9), guide = "none") +
    ggplot2::labs(x = xlab, y = ylab, title = main,
                  subtitle = pr_metareg_sub(fit)) +
    pr_theme()

  if (result) {
    cat("Meta-regression of ", x$label, " on ", moderator, "\n\n", sep = "")
    print(summary(fit))
    cat("\nThe covariate is a property of the study, so this compares ",
        "studies and\nnot patients.\n", sep = "")
  }
  spec <- pr_file_spec(file.name, path, filetype, width, height)
  pr_write(p, spec, width = 7.4, height = 5.4)
  if (is.null(spec)) print(p)
  invisible(structure(p, fit = fit))
}

#' @keywords internal
#' @noRd
pr_metareg_line <- function(fit, mod) {
  g <- seq(min(mod), max(mod), length.out = 100)
  pr <- stats::predict(fit, newmods = g)
  data.frame(mod = g, fit = as.numeric(pr$pred),
             lo = as.numeric(pr$ci.lb), hi = as.numeric(pr$ci.ub))
}

#' @keywords internal
#' @noRd
pr_metareg_sub <- function(fit) {
  b <- as.numeric(fit$b)[2]
  p <- as.numeric(fit$pval)[2]
  r2 <- tryCatch(as.numeric(fit$R2), error = function(e) NA_real_)
  s <- sprintf("Slope %.4f per unit, p = %.3f", b, p)
  if (is.finite(r2)) s <- paste0(s, sprintf("; R2 = %.0f%% of the between-study variance", r2))
  s
}
