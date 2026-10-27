# lab4ridge

<!-- badges: start -->
[![R-CMD-check](https://github.com/jexoss/lab4ridge/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jexoss/lab4ridge/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`lab4ridge` implements multiple linear regression (`linreg`) and ridge
regression (`ridgereg`) using the QR decomposition, stored as reference
class objects with `print()`, `predict()` and `coef()` methods. The package
also includes `visualize_airport_delays()`, which uses dplyr and ggplot2 to
plot mean flight delays by airport location.

## Installation

You can install the development version of lab4ridge from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("jexoss/lab4ridge")
```

## Example

Fit a ridge regression model and inspect the results:

``` r
library(lab4ridge)

mod <- ridgereg$new(Petal.Length ~ Sepal.Width + Sepal.Length,
                    data = iris, lambda = 1)
mod$print()
mod$coef()
head(mod$predict())
```

Visualize mean arrival delays for flights departing New York City in 2013:

``` r
visualize_airport_delays()
```

See the vignettes for a full example of using `ridgereg()` with the caret
package:

``` r
browseVignettes("lab4ridge")
```
