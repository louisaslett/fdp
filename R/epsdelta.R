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
epsdelta <- function(epsilon, delta = 0L) {
  mid <- (1.0 - delta) / (exp(epsilon) + 1.0)
  x <- data.frame(alpha = c(0.0, mid, 1.0 - delta),
                  beta = c(1.0 - delta, 1.0 - delta - mid * exp(epsilon), 0.0))
  attr(x, "fdp_name") <- paste0("(", epsilon, ",", delta, ")-DP")
  attr(x, "fdp_draw") <- "line"
  x
}
