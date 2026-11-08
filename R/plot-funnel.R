#' A funnel of the studies, with a test of asymmetry
#'
#' Asymmetry in a prognostic review is read with more care than in a
#' treatment review. Small validations are not only more variable, they are
#' often done in a different kind of centre, so a gap on one side can be
#' case mix rather than suppression. The test is reported and the caveat is
#' printed with it.
#'
#' @inheritParams easypr_forest
#' @param levels Pseudo confidence contours to draw, as percentages.
#' @return The ggplot, invisibly, with the asymmetry test attached as
#'   the `"test"` attribute.
#' @examples
#' \donttest{
#' data(EuroSCORE, package = "metamisc")
#' easypr_funnel(easypr(EuroSCORE, sm = "cstat"))
#' }
#' @export
easypr_funnel <- function(x, main = NULL, xlab = NULL, result = TRUE,
                          levels = c(95, 99),
                          filetype = c("png", "pdf", "tiff"), path = NULL,
                          file.name = NULL, width = NULL, height = NULL) {
  pr_check(x, "easypr_funnel()")
  t <- x$table
  if (nrow(t) < 10L) {
    message("easypr: ", nrow(t), " ", pr_plural(x$what), ". A test of funnel asymmetry ",
            "has little power below about ten, so read it as a description ",
            "and not as evidence of absence.")
  }
  ft <- tryCatch(metamisc::fat(b = t$est, b.se = t$se, method = "E-FIV"),
                 error = function(e) NULL)

  se_max <- max(t$se, na.rm = TRUE) * 1.05
  grid <- data.frame(se = seq(0, se_max, length.out = 120))
  bands <- do.call(rbind, lapply(levels, function(L) {
    z <- stats::qnorm(1 - (1 - L / 100) / 2)
    data.frame(se = grid$se, lo = x$estimate - z * grid$se,
               hi = x$estimate + z * grid$se, level = paste0(L, "%"))
  }))

  if (is.null(xlab)) xlab <- x$label
  if (is.null(main)) main <- paste0("Funnel of the ", pr_plural(x$what), ", ", x$label)

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = bands,
                         ggplot2::aes(y = se, xmin = lo, xmax = hi,
                                      fill = level), alpha = 0.18) +
    ggplot2::scale_fill_manual(values = c("grey55", "grey75"), name = NULL) +
    ggplot2::geom_vline(xintercept = x$estimate, linetype = 2,
                        colour = "#1b3a5c") +
    ggplot2::geom_point(data = t, ggplot2::aes(x = est, y = se),
                        size = 2.1, colour = "#1b3a5c") +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(x = xlab, y = "Standard error", title = main,
                  subtitle = pr_fat_line(ft, nrow(t), x$what)) +
    pr_theme()

  if (result && !is.null(ft)) print(ft)
  spec <- pr_file_spec(file.name, path, filetype, width, height)
  pr_write(p, spec, width = 7.2, height = 5.4)
  if (is.null(spec)) print(p)
  invisible(structure(p, test = ft))
}

#' @keywords internal
#' @noRd
pr_fat_line <- function(ft, k, what = "study") {
  n <- sprintf("%d %s", k, pr_plural(what, k))
  if (is.null(ft)) return(n)
  p <- tryCatch(as.numeric(ft$pval), error = function(e) NA_real_)
  if (!is.finite(p)) return(n)
  sprintf("%s; test of asymmetry p = %.3f%s", n, p,
          if (k < 10) ", too few studies to interpret" else "")
}
