#' Convert Gaussian differential privacy to classical (epsilon, delta)-differential privacy
#'
#' @description
#' Computes the exact \eqn{(\varepsilon, \delta)}-differential privacy guarantee corresponding to a given \eqn{\mu}-Gaussian differential privacy (GDP) mechanism for a specified \eqn{\varepsilon} value.
#' This conversion is based on the closed-form relationship established in Corollary 1 (p.16) of Dong et al. (2022), which provides the tightest possible \eqn{\delta} for any given \eqn{\varepsilon} and \eqn{\mu}.
#'
#' @details
#' While GDP provides a complete characterisation of privacy through the trade-off function, classical \eqn{(\varepsilon, \delta)}-differential privacy remains the most widely recognised privacy definition in both theoretical and applied research.
#' This function enables practitioners to translate GDP guarantees into the more familiar \eqn{(\varepsilon, \delta)}-DP language.
#'
#' For a mechanism satisfying \eqn{\mu}-GDP, the exact \eqn{(\varepsilon, \delta)}-DP guarantee is given by Corollary 1 of Dong et al. (2022):
#' \deqn{\delta(\varepsilon, \mu) = \Phi\left(-\frac{\varepsilon}{\mu} + \frac{\mu}{2}\right) - e^\varepsilon \Phi\left(-\frac{\varepsilon}{\mu} - \frac{\mu}{2}\right)}
#' where \eqn{\Phi} denotes the cumulative distribution function of the standard Normal distribution.
#' This was a result originally proved in Balle and Wang (2018).
#'
#' @references
#' Balle, B. and Wang, Y-X. (2018). “Improving the Gaussian Mechanism for Differential Privacy: Analytical Calibration and Optimal Denoising”. _Proceedings of the 35th International Conference on Machine Learning_, **80**, 394–403. Available at: \url{https://proceedings.mlr.press/v80/balle18a.html}.
#'
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' @param mu
#'        Numeric scalar specifying the \eqn{\mu} parameter of the Gaussian differential privacy mechanism.
#'        Must be non-negative.
#' @param epsilon
#'        Numeric scalar specifying the target \eqn{\varepsilon} privacy parameter.
#'        Must be non-negative.
#'        The function computes the minimal \eqn{\delta} such that \eqn{\mu}-GDP implies \eqn{(\varepsilon, \delta)}-DP.
#' @param dp
#'        Optional integer specifying the number of decimal places for rounding the computed \eqn{\delta} value.
#'        If provided, \eqn{\delta} is rounded *up* to ensure the privacy guarantee remains valid.
#'        If `NULL` (default), the exact value is returned without rounding.
#'        Must be a positive integer if specified.
#'
#' @return
#' A \eqn{(\varepsilon, \delta)}-DP trade-off function object (see [epsdelta()]) of class `c("fdp_epsdelta_tradeoff", "function")`.
#'
#' @seealso
#' [gdp()] for constructing Gaussian differential privacy trade-off functions,
#' [epsdelta()] for directly constructing \eqn{(\varepsilon, \delta)}-DP trade-off functions,
#' [est_gdp()] for estimating \eqn{\mu} from empirical trade-off functions,
#' [est_epsdelta()] for estimating \eqn{(\varepsilon, \delta)} from empirical trade-off functions,
#' [fdp()] for plotting and comparing trade-off functions.
#'
#' @export
#'
#' @examples
#' # Convert mu = 1 GDP to (epsilon, delta)-DP with epsilon = 1
#' dp_guarantee <- gdp_to_epsdelta(mu = 1.0, epsilon = 1.0)
#' dp_guarantee
#'
#' # Round delta to 6 decimal places for reporting
#' dp_rounded <- gdp_to_epsdelta(mu = 1.0, epsilon = 1.0, dp = 6)
#' dp_rounded
#'
#' # Compare the original GDP with its (epsilon, delta)-DP representation
#' fdp(gdp(1.0),
#'     gdp_to_epsdelta(mu = 1.0, epsilon = 1.0),
#'     .legend = "Privacy Mechanism")
#'
#' # Explore how delta varies with epsilon for a fixed mu
#' mu_fixed <- 1.0
#' epsilons <- c(0.1, 0.5, 1.0, 2.0)
#' 
#' res <- fdp(gdp(mu_fixed))
#' for (eps in epsilons) {
#'   res <- res+fdp(gdp_to_epsdelta(mu = mu_fixed, epsilon = eps))
#' }
#' res
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
