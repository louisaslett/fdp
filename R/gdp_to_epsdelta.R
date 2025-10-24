#' Exact (epsilon, delta)-differential privacy of GDP
#'
#' Calculated exact (epsilon, delta)-differential privacy of a GDP using Corollary 1, p.16, in Dong et al. (2022).
#'
#' TODO description
#'
#' @references
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' @param mu
#'        The parameter of GDP to match
#' @param epsilon
#'        The fixed target epsilon to use, with bounding computed by finding delta.
#' @param dp
#'        The number of decimal places of accuracy to use when estimating the delta parameter.
#'
#' @return
#' An
#'
#' @export
#'
#' @examples
#' #gdp(0.5)
gdp_to_epsdelta <- function(mu = 0.5, epsilon = 1.0, dp = NULL) {
  check_scalar(mu, min = 0.0)
  check_scalar(epsilon, min = 0.0)
  if (!is.null(dp)) {
    dp <- as.integer(dp)
    check_scalar(dp, min = 1L)
  }
  delta <- stats::pnorm(-epsilon / mu + mu / 2.0) - exp(epsilon) * stats::pnorm(-epsilon / mu - mu / 2.0)
  if (!is.null(dp)) {
    delta <- ceiling(delta * 10.0^dp) * 10.0^(-dp)
  }
  epsdelta(epsilon = epsilon, delta = delta)
}
