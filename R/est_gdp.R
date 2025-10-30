#' Gaussian differential privacy parameters lower bounding empirical trade-off points
#'
#' @description
#' Estimates the minimal Gaussian differential privacy (GDP) parameter \eqn{\mu} that provides a valid lower bound for a collection of empirical or analytically-derived trade-off points.
#' **Note:** due to the numerical optimisation involved, this is only an approximation.
#'
#' @details
#' Given a set of trade-off points \eqn{\{(\alpha_i, \beta_i)\}_{i=1}^n} representing Type-I and Type-II error rates, this function numerically solves for the smallest \eqn{\mu \ge 0} such that the \eqn{\mu}-GDP trade-off function
#' \deqn{G_\mu(\alpha) = \Phi\left(\Phi^{-1}(1-\alpha) - \mu\right)}
#' satisfies \eqn{G_\mu(\alpha_i) \le \beta_i} for all \eqn{i = 1, \ldots, n}, where \eqn{\Phi} denotes the standard normal cumulative distribution function.
#'
#' **Warning:** since this is a numerical optimisation on a finite set of trade-off points, there is no mathematical guarantee of correctness.
#' As such, the \eqn{\mu} found ought best to be viewed as an approximate lower bound on the true values, since there could be intermediate trade-off points that are not supplied which cause the true values to be larger.
#'
#' This function may be useful for post-hoc privacy auditing, privacy budget allocation, or mechanism comparison.
#'
#' @references
#' Dong, J., Roth, A. and Su, W.J. (2022). “Gaussian Differential Privacy”. _Journal of the Royal Statistical Society Series B_, **84**(1), 3–37. \doi{10.1111/rssb.12454}.
#'
#' @param x
#'        One or more f-DP trade-off specifications to be lower bounded. Accepts the same flexible input types as [fdp()]:
#'        \itemize{
#'          \item A function (user-defined or built-in, e.g., [lap()]) that when called with a numeric vector `alpha` returns a data frame with columns `alpha` and `beta`;
#'          \item A data frame with columns `alpha` and `beta` containing empirical trade-off points;
#'          \item A numeric vector of length 101 (interpreted as `beta` values on the canonical grid `alpha = seq(0, 1, by = 0.01)`).
#'        }
#'        The function extracts all Type-I/Type-II error coordinates and finds the minimal \eqn{(\varepsilon, \delta)}-DP parameters lower bounding them.
#' @param dp
#'        Integer scalar specifying the number of decimal places of precision for the result (with careful rounding employed to ensure the bound holds).
#'        Must be a non-negative integer.
#'        Default is `2L`.
#'
#' @return
#' A GDP trade-off function object (see [gdp()]) with class `c("fdp_gdp_tradeoff", "function")`.
#' This represents the tightest \eqn{\mu}-GDP trade-off function that lower bounds the input `x`.
#'
#' @seealso
#' [gdp()] for constructing GDP trade-off functions with known \eqn{\mu},
#' [fdp()] for visualising and comparing trade-off functions,
#' [gdp_to_epsdelta()] for converting from GDP to classical \eqn{(\varepsilon, \delta)}-DP,
#' [est_epsdelta()] for estimating classical DP parameters from trade-off points.
#'
#' @export
#'
#' @examples
#' # Estimate GDP from manually specified empirical trade-off points
#' # These could come from empirical measurements or privacy audits
#' empirical_points <- data.frame(
#'   alpha = c(0.00, 0.05, 0.10, 0.25, 0.50, 1.00),
#'   beta  = c(1.00, 0.93, 0.87, 0.72, 0.43, 0.00)
#' )
#' result <- est_gdp(empirical_points)
#' result
#'
#' # Visualise how well the GDP bound fits the empirical points
#' fdp(empirical_points, result)
#'
#' # Find the GDP lower bound for a Laplace mechanism.
#' lap_mechanism <- lap(1.5)
#' gdp_bound <- est_gdp(lap_mechanism)
#' gdp_bound
#'
#' # Compare the Laplace mechanism with its GDP lower bound
#' fdp(lap_mechanism, gdp_bound)
#'
#' # Control precision with the dp parameter
#' result_1dp <- est_gdp(empirical_points, dp = 1L)
#' result_3dp <- est_gdp(empirical_points, dp = 3L)
#' # Higher precision gives tighter bounds
#' fdp(empirical_points, result_1dp, result_3dp)
est_gdp <- function(x, dp = 2L) {
  check_scalar(dp, min = 0L)
  if (!is.integer(dp)) cli::cli_abort(c(x = "{.code dp} argument should be a non-negative integer, indicating the decimal places of accuracy to computing bounding DP to."))

  alpha <- seq(0.0, 1.0, by = 0.01)

  target <- preprocess_args(list(est_gdp = substitute(x)), alpha)[[1L]]
  g <- function(mu, target) { # if -'ve, no bound; if +'ve, bounds.
    x <- target[["beta"]] - gdp(mu)(target[["alpha"]])[["beta"]]
    if (any(x < 0.0)) {
      return(sum(x[x < 0.0]))
    }
    sum(x[x > 0.0])
  }
  if (min(g(10.0, target)) < 0.0) {
    cli::cli_abort("Unable to find a \u03BC < 10 which lower bounds provided trade-off points. May not be bounded by Gaussian differential privacy trade-off function.")
  }
  if (min(g(10.0^(-dp), target)) > 0.0) {
    return(gdp(10.0^(-dp)))
  }
  mu <- stats::uniroot(g,
                       target = target,
                       lower = 10.0^(-dp), upper = 10.0)$root
  mu <- ceiling(mu * 10.0^dp) * 10.0^(-dp)
  while (min(g(mu, target)) < 0.0) {
    mu <- mu + 10.0^(-dp)
  }
  gdp(mu)
}
