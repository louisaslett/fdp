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
#' A GDP function
#'
#' @export
#'
#' @examples
#' #gdp(0.5)
gdp <- function(mu = 1.0) {
  f <- function(alpha) {
    if (missing(alpha)) {
      x <- data.frame(alpha = seq(0.0, 1.0, by = 0.01))
    } else {
      check_alpha(alpha)
      x <- data.frame(alpha = alpha)
    }
    x$beta <- stats::pnorm(stats::qnorm(1.0 - x$alpha) - mu)
    x <- fdp_name(x, paste0(mu, "-GDP"))
    x
  }
  f <- fdp_name(f, paste0(mu, "-GDP"))
  class(f) <- c("fdp_gdp_tradeoff", class(f))
  f
}

#' @exportS3Method print fdp_gdp_tradeoff
print.fdp_gdp_tradeoff <- function(x, ...) {
  cat(paste0("Gaussian Differential Privacy Trade-off Function\n  Parameters:\n    mu = ", get("mu", envir = environment(x)), "\n"))
  invisible()
}
