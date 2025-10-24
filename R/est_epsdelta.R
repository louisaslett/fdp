#' Estimate (epsilon, delta)-differential privacy of trade off points
#'
#' Estimates the (epsilon, delta)-differential privacy of a set of trade off points
#'
#' TODO description
#'
#' @references
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' @param x
#'        Add description from fdp to here.
#' @param epsilon
#'        If specified, is the fixed epsilon to use with bounding attempted by varying delta.
#' @param delta
#'        If specified, is the fixed delta to use with bounding attempted by varying epsilon.
#' @param dp
#'        The number of decimal places of accuracy to use when estimating the parameters of (epsilon, delta)-differential privacy.
#'
#' @return
#' An
#'
#' @export
#'
#' @examples
#' #gdp(0.5)
est_epsdelta <- function(x, epsilon = NULL, delta = NULL, dp = 2L) {
  if (is.null(epsilon) && is.null(delta)) {
    cli::cli_abort(c(x = "At least one of {.code epsilon} or {.code delta} arguments must be specified."))
  }
  if (!is.null(epsilon)) check_scalar(epsilon, min = 0.0)
  if (!is.null(delta)) check_scalar(delta, min = 0.0, max = 1.0)
  check_scalar(dp, min = 0L)
  if (!is.integer(dp)) cli::cli_abort(c(x = "{.code dp} argument should be a non-negative integer, indicating the decimal places of accuracy to computing bounding DP to."))

  alpha <- seq(0.0, 1.0, by = 0.01)

  target <- preprocess_args(list(est_epsdelta = substitute(x)), alpha)[[1L]]
  g <- function(epsilon, delta, target) {
    x <- target[["beta"]] - epsdelta(epsilon, delta)(target[["alpha"]])[["beta"]]
    if (any(x < 0.0)) {
      return(sum(x[x < 0.0]))
    }
    sum(x[x > 0.0])
  }

  if (is.null(epsilon)) {
    epsilon <- find_epsilon(delta, target, dp, g)
  } else {
    delta <- find_delta(epsilon, target, dp, g)
  }

  epsdelta(epsilon, delta)
}

find_epsilon <- function(delta, target, dp, g) {
  if (g(30.0, delta, target) < 0.0) {
    cli::cli_abort("Unable to find any \u03B5 < 30 which lower bounds provided trade-off points. May not be bounded by (\u03B5, \u03B4 = {delta})-differential privacy trade-off function.")
  }
  if (g(10.0^(-dp), delta, target) > 0.0) {
    return(10.0^(-dp))
  }

  epsilon <- stats::uniroot(g,
                            delta = delta,
                            target = target,
                            lower = 10.0^(-dp), upper = 30.0)$root
  epsilon <- ceiling(epsilon * 10.0^dp) * 10.0^(-dp)
  while (g(epsilon, delta, target) < -.Machine$double.eps) {
    epsilon <- epsilon + 10.0^(-dp)
  }
  epsilon
}

find_delta <- function(epsilon, target, dp, g) {
  if (g(epsilon, 1.0, target) < 0.0) {
    cli::cli_abort("Unable to find a \u03B4 < 1.0 which lower bounds provided trade-off points. May not be bounded by (\u03B5 = {epsilon}, \u03B4)-differential privacy trade-off function.")
  }
  if (g(epsilon, 0.0, target) > 0.0) {
    return(0.0)
  }

  delta <- stats::uniroot(g,
                          epsilon = epsilon,
                          target = target,
                          lower = 0.0, upper = 1.0)$root
  delta <- ceiling(delta * 10.0^dp) * 10.0^(-dp)
  while (g(epsilon, delta, target) < -.Machine$double.eps) {
    delta <- delta + 10.0^(-dp)
  }
  delta
}
