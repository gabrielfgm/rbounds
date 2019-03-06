<!-- README.md is generated from README.Rmd. Please edit that file -->
rbounds: Estimating Bounds on Conditional Expectation Functions
===============================================================

The goal of rbounds is to estimate bounds on the location of parameters
in models in which the value of the parameter can only be partially
identified.

The package was developed to estimate the conditional mean of a binary
outcome with non-random missing outcomes. At the moment the package
contains one function which estimates bounds on a conditional mean using
non-parametric kernel regression estimators from the `np` package. In
the future I hope to extend the package to cover treatment effect
estimation, and to work with some additional assumptions such as
monotone regression and monotone treatment response.

Example of bounding the conditional mean
----------------------------------------

Imagine that we are interested in estimating *E*(*y*\|*x*) where
*y* ∈ {0, 1}. Unfortunately, we are missing some observations of *y*.
Whether an observation is missing is denoted by a dummy variable *z*,
where when *z* = 1 *y* is observed and otherwise the value of *y* is not
observed. We have some covariates *x* whose value we observe for all
observations.

By the law of total probability it follows that

*E*(*y*\|*x*) = *E*(*y*\|*x*, *z* = 1)*P*(*z* = 1\|*x*) + *E*(*y*\|*x*, *z* = 0)*P*(*z* = 0\|*x*).

Each of the pieces of this equation can be estimated from the available
data with the exception of *E*(*y*\|*x*, *z* = 0), which is the
conditional expectation of the outcome for the cases in which the
outcome value is unobserved.

However, since 0 ≤ *y* ≤ 1, the unobserved function can take on the
value of at most 1 – if all the unobserved outcomes were 1, or at least
0. Thus the true conditional mean must fall within the bounds:

*E*(*y*\|*x*, *z* = 1)*P*(*z* = 1\|*x*) ≤ *E*(*y*\|*x*) ≤ *E*(*y*\|*x*, *z* = 1)*P*(*z* = 1\|*x*) + (1 − *P*(*z* = 1\|*x*)).

More generically, if we denote the minimum value the conditional outcome
can take on as *γ*<sub>0</sub> and the maximum by *γ*<sub>1</sub>, then
the conditional mean can be bounded by

*E*(*y*\|*x*, *z* = 1)*P*(*z* = 1\|*x*) + *γ*<sub>0</sub>*P*(*z* = 0\|*x*) ≤ *E*(*y*\|*x*) ≤ *E*(*y*\|*x*, *z* = 1)*P*(*z* = 1\|*x*) + *γ*<sub>1</sub>*P*(*z* = 0\|*x*).

The conditional expectations in these expressions can be estimated
parametrically or non-parametrically, and confidence intervals can be
derived for the locations of the endpoints.

Example
-------

Using simulated data we present a simple example of the `pidoutcomes()`
function which we use to estimate partially identified conditional
conditional means.

``` r
## We generate some fake data
library(rbounds)
#> Loading required package: np
#> Nonparametric Kernel Methods for Mixed Datatypes (version 0.60-9)
#> [vignette("np_faq",package="np") provides answers to frequently asked questions]
#> [vignette("np",package="np") an overview]
#> [vignette("entropy_np",package="np") an overview of entropy-based methods]
set.seed(42)
N <- 100
x <- rnorm(N)
z <- rbinom(N, 1, pnorm(x))
y <- rbinom(N, 1, pnorm(x))

df <- data.frame(y, x, z)

res <- pidoutcomes(y~x, z, df)
res
#> Av. Lower CI Av. Lower Bound Av. Upper Bound Av. Upper CI
#>   0.24089        0.30612         0.82252       0.88991

# We can plot it with a generic plotting method
plot(res)
```

![](README-example-1.png)
