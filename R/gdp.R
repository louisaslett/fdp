#' Gaussian differential privacy trade off function
#'
#' Creates a Gaussian differential privacy trade off function
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
#' A GDP object
#'
#' @export
#'
#' @examples
#' #gdp(0.5)
gdp <- function(mu = 1.0) {
  x <- data.frame(alpha = seq(0.0, 1.0, length.out = 100L))
  x$beta <- stats::pnorm(stats::qnorm(1.0 - x$alpha) - mu)
  attr(x, "fdp_name") <- paste0(mu, "-GDP")
  x
}
