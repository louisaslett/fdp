#' (epsilon, delta)-differential privacy trade-off function
#'
#' @description
#' Constructs the trade-off function corresponding to the classical \eqn{(\varepsilon, \delta)}-differential privacy guarantee.
#' This is the f-DP representation of the approximate differential privacy definition, which allows a small probability \eqn{\delta} of privacy breach (if \eqn{\delta > 0}) while maintaining \eqn{\varepsilon}-differential privacy with probability \eqn{1-\delta}.
#'
#' The resulting trade-off function is piecewise linear with two segments, reflecting the geometry of \eqn{(\varepsilon, \delta)}-DP in the hypothesis testing framework.
#' The function returned can be called either without arguments to retrieve the underlying data points, or with an `alpha` argument to evaluate the trade-off at specific Type-I error rates.
#'
#' @details
#' Creates an \eqn{(\varepsilon, \delta)}-differential privacy trade-off function for use in f-DP analysis and visualisation.
#' If you would like a reminder of the formal definition of \eqn{(\varepsilon, \delta)}-DP, please see further down this documentation page in the "Formal definition" Section.
#'
#' The function returns a closure that stores the \eqn{\varepsilon} and \eqn{\delta} parameters in its environment.
#' This function can be called with or without arguments supplied, either to obtain the skeleton or particular Type-II error rates for given Type-I errors respectively.
#'
#' # Formal definition
#'
#' Classical \eqn{(\varepsilon, \delta)}-differential privacy (Dwork et al., 2006a,b) states that a randomised mechanism \eqn{M} satisfies \eqn{(\varepsilon, \delta)}-DP if for all neighbouring datasets \eqn{S} and \eqn{S'} that differ in a single observation, and any event \eqn{E},
#' \deqn{\mathbb{P}(M(S) \in E) \le e^\varepsilon \mathbb{P}[M(S') \in E] + \delta}
#'
#' In the f-DP framework (Dong et al., 2022), this corresponds to a specific trade-off function \eqn{f_{\varepsilon,\delta} \colon [0,1] \to [0,1]} which maps Type-I error rates \eqn{\alpha} to the minimum achievable Type-II error rates \eqn{\beta} when distinguishing between the output distributions \eqn{M(S)} and \eqn{M(S')}.
#'
#' The special case \eqn{\delta = 0} corresponds to pure \eqn{\varepsilon}-differential privacy, where the trade-off function has no fixed disclosure risk.
#'
#' @references
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' Dwork, C., Kenthapadi, K., McSherry, F., Mironov, I. and Naor, M. (2006a) “Our Data, Ourselves: Privacy Via Distributed Noise Generation”. In: _Advances in Cryptology - EUROCRYPT 2006_, 486–503. \doi{10.1007/11761679_29}.
#' 
#' Dwork, C., McSherry, F., Nissim, K. and Smith, A. (2006b) “Calibrating Noise to Sensitivity in Private Data Analysis”. In: _Theory of Cryptography_, 265–284. \doi{10.1007/11681878_14}.
#' 
#' @param epsilon
#'        Numeric scalar specifying the \eqn{\varepsilon} privacy parameter.
#'        Must be non-negative.
#' @param delta
#'        Numeric scalar specifying the \eqn{\delta} privacy parameter.
#'        Must be in \eqn{[0, 1]}.
#'        Default is `0.0` (pure \eqn{\varepsilon}-DP).
#'
#' @return
#' A function of class `c("fdp_epsdelta_tradeoff", "function")` that computes the \eqn{(\varepsilon, \delta)}-DP trade-off function.
#'
#' When called:
#' \itemize{
#'   \item **Without arguments**: Returns a data frame with columns `alpha` and `beta` containing the skeleton points of the piecewise linear trade-off function.
#'   \item **With an `alpha` argument**: Returns a data frame with columns `alpha` and `beta` containing the Type-II error values corresponding to the specified Type-I error rates.
#' }
#'
#' @seealso
#' [fdp()] for plotting trade-off functions,
#' [est_epsdelta()] for finding the choice of \eqn{\varepsilon} and \eqn{\delta} that lower bounds a collection of trade-off functions.
#' 
#' Additional trade-off functions can be found in
#' [gdp()] for Gaussian differential privacy, and
#' [lap()] for Laplace differential privacy.
#'
#' @export
#'
#' @examples
#' # Pure epsilon-differential privacy with epsilon = 1
#' pure_dp <- epsdelta(1.0)
#' pure_dp
#' pure_dp()  # View the skeleton points
#'
#' # Approximate DP with epsilon = 1 and delta = 0.01
#' approx_dp <- epsdelta(1.0, 0.01)
#' approx_dp
#'
#' # Evaluate at specific Type-I error rates
#' approx_dp(c(0.05, 0.1, 0.25, 0.5))
#'
#' # Plot and compare different (epsilon, delta) configurations
#' fdp(epsdelta(0.5),
#'     epsdelta(1.0),
#'     epsdelta(1.0, 0.01))
#'
#' # Compare with Gaussian DP
#' fdp(epsdelta(1.0),
#'     epsdelta(1.0, 0.01),
#'     gdp(1.0),
#'     .legend = "Privacy Mechanism")
epsdelta <- function(epsilon, delta = 0.0) {
  check_scalar(epsilon, min = 0.0)
  check_scalar(delta, min = 0.0, max = 1.0)

  # Compute trade-off function skeleton
  mid <- (1.0 - delta) / (exp(epsilon) + 1.0)
  tradeoff <- unique(data.frame(alpha = c(0.0, mid, 1.0 - delta, 1.0),
                                beta  = c(1.0 - delta, 1.0 - delta - mid * exp(epsilon), 0.0, 0.0)))
  tradeoff <- fdp_line(fdp_name(tradeoff, paste0("(", epsilon, ",", delta, ")-DP")))

  # create interpolated trade-off function (linear interpolation)
  f <- function(alpha) {
    if (missing(alpha) || as.character(sys.call(sys.parent(3L)))[[1L]] == "preprocess_args") {
      tradeoff
    } else {
      res <- data.frame(alpha = alpha, beta = stats::approx(x = tradeoff[["alpha"]], y = tradeoff[["beta"]], xout = alpha)$y)
      res <- fdp_name(res, paste0("(", epsilon, ",", delta, ")-DP"))
      res
    }
  }
  # Assign attributes, class, and return function
  f <- fdp_line(fdp_name(f, paste0("(", epsilon, ",", delta, ")-DP")))
  class(f) <- c("fdp_epsdelta_tradeoff", class(f))
  f
}

#' @exportS3Method print fdp_epsdelta_tradeoff
print.fdp_epsdelta_tradeoff <- function(x, ...) {
  cat(paste0("(\u03B5, \u03B4)-Differential Privacy Trade-off Function\n  Parameters:\n    \u03B5 = ", get("epsilon", envir = environment(x)), "\n    \u03B4 = ", get("delta", envir = environment(x)), "\n"))
  invisible()
}
