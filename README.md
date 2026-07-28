# helpRfuncs

A collection of reusable R helpers for scientific data analysis, visualization,
and astronomy workflows.

## Installation

`Rfits` is hosted on GitHub; install it before installing this package:

```r
install.packages("remotes")
remotes::install_github("ASGR/Rfits")
remotes::install_github("AngusWright/helpRfuncs")
```

## Features

- **Scientific plotting:** enhanced axes and colour bars, 2D histograms,
  contours, density plots, covariance displays, triangle plots, and
  publication-ready device export.
- **Statistics:** weighted variance, standard deviation, quantiles, tabulates,
  empirical CDFs, density estimation, running statistics, and sample-tension
  metrics.
- **Astronomy tools:** coordinate conversions between equatorial, ecliptic,
  galactic, and spherical systems, Aitoff plotting, redshift distributions,
  and magnitude-limit weighting.
- **Data utilities:** flexible table, FITS, Feather, and Parquet readers and
  writers, plus filename, string, rounding, duplicate-detection, and array
  helpers.

## Quick examples

```r
library(helpRfuncs)

# Compute weighted summary statistics.
weighted.quantile(c(1, 2, 5), weights = c(1, 2, 1), probs = c(0.25, 0.5, 0.75))

# Convert equatorial coordinates to Galactic longitude and latitude.
eq2gal(alpha = 180, delta = 0)

# Draw a weighted two-dimensional histogram.
hist2D(xf = rnorm(500), yf = rnorm(500), w = rep(1, 500))
```

Run `?helpRfuncs` after installation for a reference to the exported helpers.
