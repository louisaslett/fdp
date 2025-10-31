#' Gaussian differential privacy trade-off function
#'
#' @description
#' Constructs the trade-off function corresponding to \eqn{\mu}-Gaussian differential privacy (GDP).
#' This framework, introduced by Dong et al. (2022), provides a natural privacy guarantee for mechanisms based on Gaussian noise, typically offering tighter composition properties and a better privacy-utility trade-off than classical \eqn{(\varepsilon, \delta)}-differential privacy.
#'
#' @details
#' Creates a \eqn{\mu}-Gaussian differential privacy trade-off function for use in f-DP analysis and visualisation.
#' If you would like a reminder of the formal definition of \eqn{\mu}-GDP, please see further down this documentation page in the "Formal definition" Section.
#'
#' The function returns a closure that stores the \eqn{\mu} parameter in its environment.
#' This function can be called with or without argument supplied, either to obtain points on a canonical grid or particular Type-II error rates for given Type-I errors respectively.
#'
#' # Formal definition
#'
#' Gaussian differential privacy (Dong et al., 2022) arises as the trade-off function corresponding to distinguishing between two Normal distributions with unit variance and means differing by \eqn{\mu}.
#' Without loss of generality, the trade-off function is therefore,
#' \deqn{G_\mu := T\left(N(0, 1), N(\mu, 1)\right) \quad\text{for}\quad \mu \ge 0.}
#' This leads to,
#' \deqn{G_\mu(\alpha) = \Phi\left(\Phi^{-1}(1-\alpha)-\mu\right)}
#' where \eqn{\Phi} is the standard Normal cumulative distribution function.
#'
#' The most natural way to satisfy \eqn{\mu}-GDP is by adding Gaussian noise to construct the randomised algorithm.
#' Theorem 1 in Dong et al. (2022) identifies the correct variance of that noise for a given sensitivity of the statistic to be released.
#' Let \eqn{\theta(S)} be the statistic of the data \eqn{S} which is to be released. Then the *Gaussian mechanism* is defined to be
#' \deqn{M(S) := \theta(S) + \eta}
#' where \eqn{\eta \sim N(0, \Delta(\theta)^2 / \mu^2)} and,
#' \deqn{\Delta(\theta) := \sup_{S, S'} |\theta(S) - \theta(S')|}
#' the supremum being taken over neighbouring data sets.
#' The randomised algorithm \eqn{M(\cdot)} is then a \eqn{\mu}-GDP release of \eqn{\theta(S)}.
#'
#' More generally, *any* mechanism \eqn{M(\cdot)} satisfies \eqn{\mu}-GDP if,
#' \deqn{T\left(M(S), M(S')\right) \ge G_\mu}
#' for all neighbouring data sets \eqn{S, S'}.
#' In particular, one can seek the minimal \eqn{\mu} for a collection of trade-off functions using [est_gdp()].
#'
#' @references
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' @param mu
#'        Numeric scalar specifying the \eqn{\mu} privacy parameter.
#'        Must be non-negative.
#'
#' @return
#' A function of class `c("fdp_gdp_tradeoff", "function")` that computes the \eqn{\mu}-GDP trade-off function.
#'
#' When called:
#' \itemize{
#'   \item **Without arguments**: Returns a data frame with columns `alpha` and `beta` containing points on a canonical grid (`alpha = seq(0, 1, by = 0.01)`) of the trade-off function.
#'   \item **With an `alpha` argument**: Returns a data frame with columns `alpha` and `beta` containing the Type-II error values corresponding to the specified Type-I error rates.
#' }
#'
#' @seealso
#' [fdp()] for plotting trade-off functions,
#' [est_gdp()] for finding the choice of \eqn{\mu} that lower bounds a collection of trade-off functions.
#'
#' Additional trade-off functions can be found in
#' [epsdelta()] for classical \eqn{(\varepsilon, \delta)}-differential privacy, and
#' [lap()] for Laplace differential privacy.
#'
#' @export
#'
#' @examples
#' # Gaussian DP with mu = 1
#' gdp_1 <- gdp(1.0)
#' gdp_1
#' gdp_1()  # View points on the canonical grid
#'
#' # Stronger privacy with mu = 0.5
#' gdp_strong <- gdp(0.5)
#' gdp_strong
#'
#' # Evaluate at specific Type-I error rates
#' gdp_1(c(0.05, 0.1, 0.25, 0.5))
#'
#' # Plot and compare different mu values
#' fdp(gdp(0.5),
#'     gdp(1.0),
#'     gdp(2.0))
#'
#' # Compare Gaussian DP with classical (epsilon, delta)-DP
#' fdp(gdp(1.0),
#'     epsdelta(1.0),
#'     epsdelta(1.0, 0.01),
#'     .legend = "Privacy Mechanism")
gdp <- function(mu = 1.0) {
  check_scalar(mu, min = 0.0)

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
  cat(paste0("Gaussian Differential Privacy Trade-off Function\n  Parameters:\n    \u03BC = ", get("mu", envir = environment(x)), "\n"))
  invisible()
}
