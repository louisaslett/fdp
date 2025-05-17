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
#'
#' @return
#' A ggplot2 object which will plot
#'
#' @export
#'
#' @examples
#' #fdp(epsdelta(1,0.1), gdp(1), "a"=gdp(0.5), tst=data.frame(alpha=c(1,0.5,0),beta=c(0,0.3,1)), asd=data.frame(alpha=c(1,0.4,0),beta=c(0,0.51,1)))
fdp <- function(...) {
  xx <- list(...)
  if (length(xx) == 0L)
    return(invisible(NULL))

  p <- ggplot2::ggplot() +
    ggplot2::geom_function(fun = \(x) 1.0 - x, linetype = 2L, colour = "grey") +
    ggplot2::coord_fixed(ratio = 1.0) +
    ggplot2::theme_minimal()

  lns <- pts <- data.frame()
  for (i in seq_along(xx)) {
    # Setup name and drawing type
    nm <- ifelse(!is.null(names(xx)) && nzchar(names(xx)[i]), names(xx)[i], attr(xx[[i]], "fdp_name"))
    if (is.null(nm)) {
      cli::cli_abort(c(x = "Argument {i} is unnamed and does not have an {.code fdp_name} attribute"))
    }
    attr(xx[[i]], "fdp_draw") <- attr(xx[[i]], "fdp_draw") %||% ifelse(nrow(xx[[i]]) < 100L, "point", "line")
    if (attr(xx[[i]], "fdp_draw") == "point") {
      # Points
      if (!(attr(xx[[i]], "fdp_hide_point") %||% FALSE)) {
        pts <- rbind(pts,
                     cbind(item = nm,
                           xx[[i]]))
      }
      # Lower convex hull
      lns <- rbind(lns,
                   cbind(item = nm,
                         lower_hull(xx[[i]])))
    } else if (attr(xx[[i]], "fdp_draw") == "line") {
      if (!isTRUE(all.equal(xx[[i]], lower_hull(xx[[i]])))) {
        cli::cli_abort(c(x = "Argument {i} is to be drawn as a line, but is not convex (ie not a trade-off function). Either there is an error or this should be passed with {.fn fdp_point}."))
      }
      lns <- rbind(lns,
                   cbind(item = nm,
                         xx[[i]]))
    } else {
      cli::cli_abort(c(x = "Argument {i} has unknown {.code fdp_draw} attribute set."))
    }
  }
  if (nrow(lns) > 0L) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x = .data$alpha, y = .data$beta, col = .data$item), lns)
  }
  if (nrow(pts) > 0L) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x = .data$alpha, y = .data$beta, col = .data$item), pts, size = 0.5, shape = 4L, stroke = 1.5)
  }
  p
}
