#' Estimate Gaussian differential privacy of trade off points
#'
#' Estimates the Gaussian differential privacy of a set of trade off points
#'
#' TODO description
#'
#' @references
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' @param mu
#'        The GDP privacy parameter.
#'
#' @return
#' A GDP function
#'
#' @export
#'
#' @examples
#' #gdp(0.5)
est_gdp <- function(x, dp = 2L) {
  check_scalar(dp, min = 0L)
  if (!is.integer(dp)) cli::cli_abort(c(x = "{.code dp} argument should be a non-negative integer, indicating the decimal places of accuracy to computing bounding DP to."))

  alpha <- seq(0.0, 1.0, by = 0.01)

  target <- preprocess_args(list(est_gdp = substitute(x)), alpha)[[1L]]
  g <- function(mu, target) { # if -'ve, no bound; if +'ve, bounds.
    x <- target[["beta"]] - gdp(mu)(target[["alpha"]])[["beta"]]
    if (any(x < 0.0)) {
      return(sum(x[x < 0.0]))
    }
    sum(x[x > 0.0])
  }
  if (min(g(10.0, target)) < 0.0) {
    cli::cli_abort("Unable to find a \u03BC < 10 which lower bounds provided trade-off points. May not be bounded by Gaussian differential privacy trade-off function.")
  }
  if (min(g(10.0^(-dp), target)) > 0.0) {
    return(gdp(10.0^(-dp)))
  }
  mu <- uniroot(g,
                target = target,
                lower = 10.0^(-dp), upper = 10.0)$root
  mu <- ceiling(mu * 10.0^dp) * 10.0^(-dp)
  while (min(g(mu, target)) < 0.0) {
    mu <- mu + 10.0^(-dp)
  }
  gdp(mu)
}
