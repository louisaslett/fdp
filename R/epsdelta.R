#' (epsilon, delta)-differential privacy trade off function
#'
#' Creates an \eqn{(\varepsilon, \delta)}-differential privacy trade off function
#'
#' TODO description
#'
#' @references
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' @param epsilon
#'        The \eqn{\varepsilon} privacy parameter.
#' @param delta
#'        The \eqn{\delta} privacy parameter.
#'
#' @return
#' An
#'
#' @export
#'
#' @examples
#' #gdp(0.5)
epsdelta <- function(epsilon, delta = 0.0) {
  check_scalar(epsilon, min = 0.0)
  check_scalar(delta, min = 0.0, max = 1.0)

  # Compute trade-off function skeleton
  mid <- (1.0 - delta) / (exp(epsilon) + 1.0)
  tradeoff <- unique(data.frame(alpha = c(0.0, mid, 1.0 - delta, 1.0),
                                beta  = c(1.0 - delta, 1.0 - delta - mid * exp(epsilon), 0.0, 0.0)))
  tradeoff <- fdp_line(fdp_name(tradeoff, paste0("(", epsilon, ",", delta, ")-DP")))

  # create interpolated trade-off function (linear interpolation)
  f <- function(alpha) {
    if (missing(alpha) || as.character(sys.call(sys.parent(3L)))[[1L]] == "preprocess_args") {
      tradeoff
    } else {
      res <- data.frame(alpha = alpha, beta = approx(x = tradeoff[["alpha"]], y = tradeoff[["beta"]], xout = alpha)$y)
      res <- fdp_name(res, paste0("(", epsilon, ",", delta, ")-DP"))
      res
    }
  }
  # Assign attributes, class, and return function
  f <- fdp_line(fdp_name(f, paste0("(", epsilon, ",", delta, ")-DP")))
  class(f) <- c("fdp_epsdelta_tradeoff", class(f))
  f
}

#' @exportS3Method print fdp_epsdelta_tradeoff
print.fdp_epsdelta_tradeoff <- function(x, ...) {
  cat(paste0("(\u03B5, \u03B4)-Differential Privacy Trade-off Function\n  Parameters:\n    \u03B5 = ", get("epsilon", envir = environment(x)), "\n    \u03B4 = ", get("delta", envir = environment(x)), "\n"))
  invisible()
}
