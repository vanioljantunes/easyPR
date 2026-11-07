#' A forest of the validations
#'
#' One row per validation, with the pooled estimate underneath and the
#' prediction interval drawn through it. The prediction interval is the
#' reason this plot is worth drawing rather than reading the table: it shows
#' at a glance how much wider the next study's expectation is than the
#' confidence interval around the average.
#'
#' @param x A fit from [easypr()].
#' @param main,xlab Title and x-axis label; sensible defaults are worked out
#'   from the measure.
#' @param result Print the pooled result as well as drawing the figure.
#' @param filetype,path,file.name,width,height Where to write the figure.
#'   Nothing is written unless `file.name` is given.
#'
#' @return The ggplot, invisibly.
#' @examples
#' \donttest{
#' data(EuroSCORE, package = "metamisc")
#' easypr_forest(easypr(EuroSCORE, measure = "cstat"))
#' }
#' @export
easypr_forest <- function(x, main = NULL, xlab = NULL, result = TRUE,
                          filetype = c("png", "pdf", "tiff"), path = NULL,
                          file.name = NULL, width = NULL, height = NULL) {
  pr_check(x, "easypr_forest()")
  t <- x$table
  t$w <- pr_weights(t, x$tau2)
  t <- t[order(t$est), ]
  t$studlab <- factor(pr_unique_labels(t$studlab),
                      levels = pr_unique_labels(t$studlab))

  bk <- pr_back(x)
  for (cl in c("est", "lower", "upper")) t[[cl]] <- bk(t[[cl]])
  if (is.null(xlab)) xlab <- x$label
  if (is.null(main)) main <- if (isTRUE(x$log))
    paste0("Studies of the factor, ", x$label) else
    paste0("Validations of the model, ", x$label)
  null_at <- pr_null_at(x)

  p <- ggplot2::ggplot(t, ggplot2::aes(x = est, y = studlab)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper),
                            height = 0, colour = "grey45") +
    ggplot2::geom_point(ggplot2::aes(size = w), shape = 15, colour = "#1b3a5c") +
    ggplot2::scale_size_continuous(range = c(1.6, 5), guide = "none") +
    ggplot2::labs(x = xlab, y = NULL, title = main,
                  subtitle = pr_sub(x)) +
    pr_scale_x(x) +
    pr_theme()
  if (is.finite(null_at)) {
    p <- p + ggplot2::geom_vline(xintercept = null_at, linetype = 2,
                                 colour = "grey55")
  }
  # the pooled estimate and, wider, what the next validation should expect
  p <- p +
    ggplot2::annotate("rect", xmin = bk(x$ci.lb), xmax = bk(x$ci.ub),
                      ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "#1b3a5c") +
    ggplot2::geom_vline(xintercept = bk(x$estimate), colour = "#1b3a5c",
                        linewidth = 0.7)
  if (is.finite(x$pi.lb)) {
    p <- p + ggplot2::annotate("errorbarh", y = 0.35, xmin = bk(x$pi.lb),
                               xmax = bk(x$pi.ub), height = 0.25,
                               colour = "#b4472a", linewidth = 0.7)
  }
  if (result) print(x)
  spec <- pr_file_spec(file.name, path, filetype, width, height)
  pr_write(p, spec, width = 8, height = max(3.2, 0.26 * nrow(t) + 2))
  if (is.null(spec)) print(p)
  invisible(p)
}

#' One line under the title saying what was pooled
#' @keywords internal
#' @noRd
pr_sub <- function(x) {
  b <- pr_back(x)
  s <- sprintf("Pooled %.3f (95%% CI %.3f to %.3f)", b(x$estimate),
               b(x$ci.lb), b(x$ci.ub))
  if (is.finite(x$pi.lb)) {
    s <- paste0(s, sprintf("; 95%% prediction %.3f to %.3f", b(x$pi.lb), b(x$pi.ub)))
  }
  paste0(s, sprintf("; I2 = %.0f%%", x$I2))
}
