#' (epsilon, delta)-differential privacy parameters lower bounding empirical trade-off points
#'
#' @description
#' Estimates the \eqn{(\varepsilon, \delta)}-differential privacy parameters that lower bound a given set of empirical trade-off points.
#' This function uses numerical optimisation to identify the tightest \eqn{(\varepsilon, \delta)}-DP guarantee consistent with observed Type-I/Type-II error trade-offs, holding either \eqn{\varepsilon} or \eqn{\delta} fixed whilst optimising over the other parameter.
#' **Note:** due to the numerical optimisation involved, this is only an approximation.
#'
#' @details
#' This function numerically solves an inverse problem in the f-differential privacy framework: given empirical trade-off points \eqn{\{(\alpha_i, \beta_i)\}_{i=1}^n} characterising the distinguishability between output distributions of a randomised mechanism on neighbouring datasets, find the minimal classical \eqn{(\varepsilon, \delta)}-DP parameters such that the \eqn{(\varepsilon, \delta)}-DP trade-off function lower bounds all observed points.
#'
#' **Warning:** since this is a numerical optimisation on a finite set of trade-off points, there is no mathematical guarantee of correctness.
#' As such, the \eqn{(\varepsilon, \delta)} found ought best to be viewed as an approximate lower bound on the true values, since there could be intermediate trade-off points that are not supplied which cause the true values to be larger.
#' For example, consider:
#'
#' ```{r}
#' est_epsdelta(gdp(0.5)(), delta = 0)
#' ```
#'
#' Corollary 1, p.16, in Dong et al. (2022) means the exact answer here is \eqn{(\varepsilon = 1.45, \delta = 0.000544\dots)} and that indeed there does not in general exist any finite \eqn{\varepsilon} solution for \eqn{\delta = 0}.
#'
#' **Note:** in fact, for lower bounding \eqn{\mu}-Gaussian Differential Privacy one should use [gdp_to_epsdelta()] which employs exact theoretical results from the literature!
#'
#' This function may be useful for post-hoc privacy auditing, privacy budget allocation, or mechanism comparison.
#'
#' ## Mathematical formulation
#'
#' The \eqn{(\varepsilon, \delta)}-DP trade-off function \eqn{f_{\varepsilon,\delta} \colon [0,1] \to [0,1]} is piecewise linear (see [epsdelta()]).
#' This function seeks parameters \eqn{(\varepsilon, \delta)} such that
#' \deqn{f_{\varepsilon,\delta}(\alpha_i) \le \beta_i \quad \text{for all } i = 1, \ldots, n}
#' whilst minimising either \eqn{\varepsilon} (if `delta` is fixed) or \eqn{\delta} (if `epsilon` is fixed).
#'
#' Exactly one of `epsilon` or `delta` must be specified by the user; the function then searches for the minimal value of the unspecified parameter.
#' The optimisation first verifies whether any solution exists within reasonable bounds (\eqn{\varepsilon < 30} or \eqn{\delta < 1}), then constructs an objective measuring the signed vertical distance between the empirical points and the candidate \eqn{(\varepsilon, \delta)}-DP curve.
#' A numerical root finder then seeks the parameter value where this crosses zero, with the solution rounded up to the specified decimal precision (`dp`).
#' There are then checks that the rounded bound holds numerically, with incremental adjustment if necessary to guarantee \eqn{f_{\varepsilon,\delta}(\alpha_i) \le \beta_i} for all \eqn{i} within machine precision.
#'
#' @references
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' @param x
#'        One or more f-DP trade-off specifications to be lower bounded. Accepts the same flexible input types as [fdp()]:
#'        \itemize{
#'          \item A function (user-defined or built-in, e.g., [gdp()]) that when called with a numeric vector `alpha` returns a data frame with columns `alpha` and `beta`;
#'          \item A data frame with columns `alpha` and `beta` containing empirical trade-off points;
#'          \item A numeric vector of length 101 (interpreted as `beta` values on the canonical grid `alpha = seq(0, 1, by = 0.01)`).
#'        }
#'        The function extracts all Type-I/Type-II error coordinates and finds the minimal \eqn{(\varepsilon, \delta)}-DP parameters lower bounding them.
#' @param epsilon
#'        Optional numeric scalar specifying a fixed value of \eqn{\varepsilon \ge 0}.
#'        If supplied, the function searches for the minimal \eqn{\delta \in [0,1]} such that the \eqn{(\varepsilon, \delta)}-DP trade-off lower bounds `x`.
#'        Exactly one of `epsilon` or `delta` must be specified.
#'        Default is `NULL`.
#' @param delta
#'        Optional numeric scalar specifying a fixed value of \eqn{\delta \in [0, 1]}.
#'        If supplied, the function searches for the minimal \eqn{\varepsilon \ge 0} such that the \eqn{(\varepsilon, \delta)}-DP trade-off lower bounds `x`.
#'        Exactly one of `epsilon` or `delta` must be specified.
#'        Default is `NULL`.
#' @param dp
#'        Integer scalar specifying the number of decimal places of precision for the result (with careful rounding employed to ensure the bound holds).
#'        Must be a non-negative integer.
#'        Default is `2L`.
#'
#' @return
#' A function of class `c("fdp_epsdelta_tradeoff", "function")`, as returned by [epsdelta()], representing the tightest \eqn{(\varepsilon, \delta)}-DP trade-off function that lower bounds the input `x`.
#'
#' @seealso
#' [epsdelta()] for constructing \eqn{(\varepsilon, \delta)}-DP trade-off functions with known parameters,
#' [est_gdp()] for the analogous estimation problem in the Gaussian DP framework,
#' [fdp()] for plotting and comparing trade-off functions.
#'
#' For lower bounding \eqn{\mu}-Gaussian Differential Privacy, see [gdp_to_epsdelta()] which uses exact theoretical results from the literature.
#'
#' @export
#'
#' @examples
#' # Estimate epsilon given fixed delta for empirical trade-off points
#' # Note: unrealistically small set of points, in practice this would be a
#' #       collection of potentially thousands of points representing multiple
#' #       trade-off functions, the collection of which should be lower bounded.
#' empirical <- data.frame(
#'   alpha = c(0.00, 0.05, 0.10, 0.25, 0.50, 1.00),
#'   beta = c(1.00, 0.92, 0.85, 0.70, 0.45, 0.00)
#' )
#' result <- est_epsdelta(empirical, delta = 0.01)
#' result  # Print the estimated parameters
#'
#' # Estimate delta given fixed epsilon
#' result2 <- est_epsdelta(empirical, epsilon = 1.0)
#' result2
#'
#' # Visualise the fit
#' fdp(empirical, result, .legend = "Trade-off")
#'
#' # Find epsilon bounding a Gaussian DP mechanism with delta = 0.1 and compare
#' # with the exactly computed values
#' gdp_mechanism <- gdp(1.1)
#' approx_dp <- est_epsdelta(gdp_mechanism, delta = 0.1)
#' dp <- gdp_to_epsdelta(1.1, environment(approx_dp)$epsilon)
#' fdp(gdp_mechanism, approx_dp, dp,
#'     .legend = "Mechanism")
#'
#' # Compare precision levels
#' result_2dp <- est_epsdelta(empirical, delta = 0.01, dp = 2L)
#' result_4dp <- est_epsdelta(empirical, delta = 0.01, dp = 4L)
#' fdp(empirical, result_2dp, result_4dp)
est_epsdelta <- function(x, epsilon = NULL, delta = NULL, dp = 2L) {
  if (is.null(epsilon) && is.null(delta)) {
    cli::cli_abort(c(x = "At least one of {.code epsilon} or {.code delta} arguments must be specified."))
  }
  if (!is.null(epsilon)) check_scalar(epsilon, min = 0.0)
  if (!is.null(delta)) check_scalar(delta, min = 0.0, max = 1.0)
  check_scalar(dp, min = 0L)
  if (!is.integer(dp)) cli::cli_abort(c(x = "{.code dp} argument should be a non-negative integer, indicating the decimal places of accuracy to computing bounding DP to."))

  alpha <- seq(0.0, 1.0, by = 0.01)

  target <- preprocess_args(list(est_epsdelta = substitute(x)), alpha)[[1L]]
  g <- function(epsilon, delta, target) {
    x <- target[["beta"]] - epsdelta(epsilon, delta)(target[["alpha"]])[["beta"]]
    if (any(x < 0.0)) {
      return(sum(x[x < 0.0]))
    }
    sum(x[x > 0.0])
  }

  if (is.null(epsilon)) {
    epsilon <- find_epsilon(delta, target, dp, g)
  } else {
    delta <- find_delta(epsilon, target, dp, g)
  }

  epsdelta(epsilon, delta)
}

find_epsilon <- function(delta, target, dp, g) {
  if (g(30.0, delta, target) < 0.0) {
    cli::cli_abort("Unable to find any \u03B5 < 30 which lower bounds provided trade-off points. May not be bounded by (\u03B5, \u03B4 = {delta})-differential privacy trade-off function.")
  }
  if (g(10.0^(-dp), delta, target) > 0.0) {
    return(10.0^(-dp))
  }

  epsilon <- stats::uniroot(g,
                            delta = delta,
                            target = target,
                            lower = 10.0^(-dp), upper = 30.0)$root
  epsilon <- ceiling(epsilon * 10.0^dp) * 10.0^(-dp)
  while (g(epsilon, delta, target) < -.Machine$double.eps) {
    epsilon <- epsilon + 10.0^(-dp)
  }
  epsilon
}

find_delta <- function(epsilon, target, dp, g) {
  if (g(epsilon, 1.0, target) < 0.0) {
    cli::cli_abort("Unable to find a \u03B4 < 1.0 which lower bounds provided trade-off points. May not be bounded by (\u03B5 = {epsilon}, \u03B4)-differential privacy trade-off function.")
  }
  if (g(epsilon, 0.0, target) > 0.0) {
    return(0.0)
  }

  delta <- stats::uniroot(g,
                          epsilon = epsilon,
                          target = target,
                          lower = 0.0, upper = 1.0)$root
  delta <- ceiling(delta * 10.0^dp) * 10.0^(-dp)
  while (g(epsilon, delta, target) < -.Machine$double.eps) {
    delta <- delta + 10.0^(-dp)
  }
  delta
}
