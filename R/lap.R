#' Laplace differential privacy trade off function
#'
#' Creates a Laplace differential privacy trade off function
#'
#' TODO description
#'
#' @references
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' @param mu
#'        The Laplace privacy parameter.
#'
#' @return
#' A Laplace trade-off function
#'
#' @export
#'
#' @examples
#' #lap(1)
lap <- function(mu = 1.0) {
  check_scalar(mu, min = 0.0)

  f <- function(alpha) {
    if (missing(alpha)) {
      x <- data.frame(alpha = seq(0.0, 1.0, by = 0.01))
    } else {
      check_alpha(alpha)
      x <- data.frame(alpha = alpha)
    }
    x$beta <- ifelse(alpha < exp(-mu)/2,
                     1 - alpha * exp(mu),
                     ifelse(alpha <= 0.5,
                            exp(-mu)/(4*alpha),
                            exp(-mu)*(1-alpha)))
    x <- fdp_name(x, paste0(mu, "-Laplace"))
    x
  }
  f <- fdp_name(f, paste0(mu, "-Laplace"))
  class(f) <- c("fdp_lap_tradeoff", class(f))
  f
}

#' @exportS3Method print fdp_lap_tradeoff
print.fdp_lap_tradeoff <- function(x, ...) {
  cat(paste0("Laplace Differential Privacy Trade-off Function\n  Parameters:\n    \u03BC = ", get("mu", envir = environment(x)), "\n"))
  invisible()
}
