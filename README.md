# 📦 `{fdp}` R package

[![Project Status](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E%3D%203%29-brightgreen.svg?style=flat)](https://www.gnu.org/licenses/gpl-2.0.html)
[![fdp status badge](https://louisaslett.r-universe.dev/badges/fdp)](https://louisaslett.r-universe.dev/fdp)

The `{fdp}` package provides tools for working with $f$-differential privacy, a powerful framework that generalises traditional differential privacy definitions.
The package allows you to construct, visualise, and analyse trade-off functions that show the optimal relationship between Type I and Type II errors when distinguishing between outputs from neighboring datasets.
This is an initial release with basic functionality that may be extended over time with composition results and privacy accountants.
At present it hopefully helps researchers in the area quickly produce the $f$-DP visualisations used in papers etc.
Please cite this software if you use it.

The package is based on the $f$-differential privacy/Gaussian differential privacy framework introduced by Dong, Roth, and Su (2022) in "Gaussian Differential Privacy" (_Journal of the Royal Statistical Society Series B_, <https://doi.org/10.1111/rssb.12454>).
A nice overview of Gaussian differential privacy is available in Gomez _et al._ (2025) <https://arxiv.org/abs/2503.10945>.

## Installation

You can install the package from CRAN:

```r
install.packages("fdp")
```

Or install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("louisaslett/fdp")
```

## Example

Here are some basic examples of using the `{fdp}` package:

### Plotting Gaussian Differential Privacy

```r
library(fdp)

# Plot a single GDP trade-off function with μ = 1
fdp(gdp(1))
```

### Comparing Privacy Mechanisms

```r
# Compare Gaussian DP with classical (ε, δ)-DP
fdp(
  "Gaussian DP" = gdp(1.0),
  "Classical DP" = epsdelta(1.0),
  "Approximate DP" = epsdelta(1.3, 0.05),
  .legend = "Privacy Mechanism"
)
```

### Estimating Privacy Parameters

```r
# Define empirical trade-off points from a privacy audit
empirical_points <- data.frame(
  alpha = c(0.00, 0.05, 0.10, 0.25, 0.50, 1.00),
  beta  = c(1.00, 0.93, 0.87, 0.72, 0.43, 0.00)
)

# Find the GDP parameters that lower bound these points
gdp_bound <- est_gdp(empirical_points)
gdp_bound

# Visualize the fit
fdp(empirical_points, gdp_bound)
```

### Working with the Laplace Mechanism

```r
# Create trade-off function for Laplace mechanism with scale parameter 1.5
lap_mechanism <- lap(1.5)

# Find its GDP lower bound
gdp_equiv <- est_gdp(lap_mechanism)

# Compare them
fdp(lap_mechanism, gdp_equiv)
```

For more detailed examples and documentation, see the package vignettes and function help pages.
