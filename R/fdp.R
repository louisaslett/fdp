#' Plot f-differential privacy
#'
#' Plots an f-differential privacy trade off function
#'
#' TODO description
#'
#' @references
#' Andrew, A. M. (1979). “Another efficient algorithm for convex hulls in two dimensions”. _Information Processing Letters_, **9**(5), 216–219. \doi{10.1016/0020-0190(79)90072-3}.
#'
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' @param ...
#'        f-differential privacy inputs to plot.
#' @param .legend
#'        A title for the colour legend, by default there is no title.
#'
#' @return
#' A ggplot2 object which will plot
#'
#' @export
#'
#' @examples
#' #fdp(epsdelta(1,0.1), gdp(1), "a"=gdp(0.5), tst=data.frame(alpha=c(1,0.5,0),beta=c(0,0.3,1)), asd=data.frame(alpha=c(1,0.4,0),beta=c(0,0.51,1)))
fdp <- function(..., .legend = NULL) {
  # Grid of alpha we evaluate on for function arguments
  alpha <- seq(0.0, 1.0, length.out = 100L)

  # Preprocess args so convert everything into values
  dotargs <- as.list(substitute(list(...)))[-1L]
  x <- preprocess_args(dotargs, alpha)
  if (length(x) == 0L)
    return(invisible(NULL))

  p <- ggplot2::ggplot() +
    ggplot2::lims(x = c(0.0, 1.0), y = c(0.0, 1.0)) +
    ggplot2::coord_fixed(ratio = 1.0) +
    ggplot2::geom_function(fun = \(xx) 1.0 - xx, linetype = 2L, colour = "grey") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Type-I error", y = "Type-II error")

  lns <- pts <- list()
  nms <- NULL
  for (i in seq_along(x)) {
    nms <- c(nms, fdp_name(x[[i]]))
    if (attr(x[[i]], "fdp_draw") == "point") {
      # Points
      if (!(attr(x[[i]], "fdp_hide_point") %||% FALSE)) {
        pts <- c(pts,
                 list(cbind(item = fdp_name(x[[i]]),
                            x[[i]])))
      }
      # Lower convex hull
      lns <- c(lns,
               list(cbind(item = fdp_name(x[[i]]),
                          lower_hull(x[[i]]))))
    } else if (attr(x[[i]], "fdp_draw") == "line") {
      if (!isTRUE(all.equal(x[[i]], lower_hull(x[[i]])))) {
        cli::cli_abort(c(x = "Argument {i} (named {attr(x[[i]], 'fdp_name')}) is to be drawn as a line, but is not convex (ie not a trade-off function). Either there is an error or this should be passed with {.fn fdp_point}."))
      }
      lns <- c(lns,
               list(cbind(item = fdp_name(x[[i]]),
                          x[[i]])))
    } else {
      cli::cli_abort(c(x = "Argument {i} (named {attr(x[[i]], 'fdp_name')}) has unknown {.code fdp_draw} attribute set."))
    }
  }
  for (i in seq_along(lns)) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x = .data$alpha, y = .data$beta, col = .data$item), lns[[i]])
  }
  for (i in seq_along(pts)) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x = .data$alpha, y = .data$beta, col = .data$item), pts[[i]], size = 0.5, shape = 4L, stroke = 1.5)
  }
  p + ggplot2::scale_colour_discrete(name = .legend, breaks = nms)
}
