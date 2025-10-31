#' Laplace differential privacy trade-off function
#'
#' @description
#' Constructs the trade-off function corresponding to \eqn{\mu}-Laplace differential privacy.
#' This corresponds to a randomised algorithm based on Laplace (double exponential) noise, which is the canonical mechanism in the original differential privacy framework (Dwork et al., 2006).
#'
#' @details
#' Creates a \eqn{\mu}-Laplace differential privacy trade-off function for use in f-DP analysis and visualisation.
#' If you would like a reminder of the formal definition of \eqn{\mu}-Laplace DP, please see further down this documentation page in the "Formal definition" Section.
#'
#' The function returns a closure that stores the \eqn{\mu} parameter in its environment.
#' This function can be called with or without argument supplied, either to obtain points on a canonical grid or particular Type-II error rates for given Type-I errors respectively.
#'
#' # Formal definition
#'
#' Laplace differential privacy arises as the trade-off function corresponding to distinguishing between two Laplace distributions with unit scale parameter and locations differing by \eqn{\mu}.
#' Without loss of generality, the trade-off function is therefore,
#' \deqn{L_\mu := T\left(\text{Lap}(0, 1), \text{Lap}(\mu, 1)\right) \quad\text{for}\quad \mu \ge 0.}
#'
#' The most natural way to satisfy \eqn{\mu}-Laplace DP is by adding Laplace noise to construct the randomised algorithm.
#' This is the canonical noise mechanism used in classical \eqn{\varepsilon}-differential privacy.
#' Let \eqn{\theta(S)} be the statistic of the data \eqn{S} which is to be released.
#' Then the *Laplace mechanism* is defined to be
#' \deqn{M(S) := \theta(S) + \eta}
#' where \eqn{\eta \sim \text{Lap}(0, \Delta(\theta) / \mu)} and,
#' \deqn{\Delta(\theta) := \sup_{S, S'} |\theta(S) - \theta(S')|}
#' the supremum being taken over neighbouring data sets.
#' The randomised algorithm \eqn{M(\cdot)} is then a \eqn{\mu}-Laplace DP release of \eqn{\theta(S)}.
#' In the classical regime, this corresponds to the Laplace mechanism which satisfies \eqn{(\varepsilon=\mu)}-differential privacy (Dwork et al., 2006).
#'
#' More generally, *any* mechanism \eqn{M(\cdot)} satisfies \eqn{\mu}-Laplace DP if,
#' \deqn{T\left(M(S), M(S')\right) \ge L_\mu}
#' for all neighbouring data sets \eqn{S, S'}.
#'
#' In the f-differential privacy framework, the canonical noise mechanism is Gaussian (see [gdp()]), but \eqn{\mu}-Laplace DP does arise as the trade-off function in the limit of the group privacy of \eqn{\varepsilon}-DP as the group size goes to infinity (see Proposition 7, Dong et al., 2022).
#'
#' @references
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' Dwork, C., McSherry, F., Nissim, K. and Smith, A. (2006) “Calibrating Noise to Sensitivity in Private Data Analysis”. In: _Theory of Cryptography_, 265–284. \doi{10.1007/11681878_14}.
#'
#' @param mu
#'        Numeric scalar specifying the \eqn{\mu} privacy parameter.
#'        Must be non-negative.
#'
#' @return
#' A function of class `c("fdp_lap_tradeoff", "function")` that computes the \eqn{\mu}-Laplace DP trade-off function.
#'
#' When called:
#' \itemize{
#'   \item **Without arguments**: Returns a data frame with columns `alpha` and `beta` containing the skeleton points of the trade-off function.
#'   \item **With an `alpha` argument**: Returns a data frame with columns `alpha` and `beta` containing the Type-II error values corresponding to the specified Type-I error rates.
#' }
#'
#' @seealso
#' [fdp()] for plotting trade-off functions.
#'
#' Additional trade-off functions can be found in
#' [gdp()] for Gaussian differential privacy, and in
#' [epsdelta()] for classical \eqn{(\varepsilon, \delta)}-differential privacy.
#'
#' @export
#'
#' @examples
#' # Laplace DP with mu = 1
#' lap_1 <- lap(1.0)
#' lap_1
#' lap_1()  # View points on the canonical grid
#'
#' # Plot and compare different mu values
#' fdp(lap(0.5),
#'     lap(1.0),
#'     lap(2.0))
#'
#' # Notice that (epsilon=1)-differential privacy is indeed 1-Laplace DP
#' # The gap between the lines is the inefficiency in the privacy
#' # characterisation of classical differential privacy
#' fdp(lap(1),
#'     epsdelta(1))
#'
#' # Compare Laplace DP with Gaussian DP and classical (epsilon, delta)-DP
#' fdp(lap(1.0),
#'     gdp(1.0),
#'     epsdelta(1.0),
#'     .legend = "Privacy Mechanism")
lap <- function(mu = 1.0) {
  check_scalar(mu, min = 0.0)

  f <- function(alpha) {
    if (missing(alpha)) {
      x <- data.frame(alpha = unique(c(0.0,
                                       exp(-mu) / 2.0,
                                       seq(min(floor(100.0 * exp(-mu) / 2.0 + 1.0) / 100.0, 0.5),
                                           0.5,
                                           by = 0.01),
                                       1.0)))
    } else {
      check_alpha(alpha)
      x <- data.frame(alpha = alpha)
    }

    # Compute beta (avoiding nested ifelse)
    x$beta <- numeric(nrow(x))
    mask1 <- x$alpha < exp(-mu) / 2.0
    mask2 <- x$alpha <= 0.5 & !mask1
    mask3 <- !mask1 & !mask2

    x$beta[mask1] <- 1.0 - x$alpha[mask1] * exp(mu)
    x$beta[mask2] <- exp(-mu) / (4.0 * x$alpha[mask2])
    x$beta[mask3] <- exp(-mu) * (1.0 - x$alpha[mask3])

    # Done! Give a pretty name and return
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
