<!-- README.md is generated from README.Rmd. Please edit that file -->
rbounds
=======

The goal of rbounds is to estimate bounds on the location of parameters
in models in which the value of the parameter cannot be uniquely
identified. It was developed to estimate the conditional mean of a
binary outcome with non-random missing outcomes. The code consists of
implementations of estimators developed by Manski (2003) in particular,
as well as other econometricians/statisticians. Functions are documented
with references toward the relevant statistical and econometric
literature.

Imagine for instance that we are interested in estimating *E*(*y*\|*x*)
where *y* ∈ (0, 1). Unfortunately, we are missing some observations of
*y*. Whether an observation is missing is denoted by a dummy variable
*z*, where when *z* = 1 *y* is observed and otherwise the value of *y*
is not observed. We observe the value of *x* for all observations –
although this can be relaxed for other estimators.

By the law of total probability it follows that
*E*(*y*\|*x*) = *E*(*y*\|*x*, *z* = 1)*P*(*z* = 1\|*x*) + *E*(*y*\|*x*, *z* = 0)*P*(*z* = 0\|*x*).
Each of the pieces of this equation can be estimated from the available
data with the exception of *E*(*y*\|*x*, *z* = 0) which is the
conditional expectation of the outcome for the cases in which the
outcome value is unobserved. However, the value that this function of
the data can taken could be at most 1 – if all the unobserved outcomes
were 1, or at least 0. Thus the true conditional mean must fall within
the bounds:
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
#> Nonparametric Kernel Methods for Mixed Datatypes (version 0.60-8)
#> [vignette("np_faq",package="np") provides answers to frequently asked questions]
#> [vignette("np",package="np") an overview]
#> [vignette("entropy_np",package="np") an overview of entropy-based methods]
set.seed(42)
N <- 100
x <- rnorm(N)
z <- rbinom(N, 1, x^2/max(x^2))
y <- rbinom(N, 1, pnorm(x))

df <- data.frame(y, x, z)

res <- pidoutcomes(y~x, z, df)
#> 
Multistart 1 of 1 |
Multistart 1 of 1 |
Multistart 1 of 1 |
Multistart 1 of 1 /
Multistart 1 of 1 |
Multistart 1 of 1 |
                   

Multistart 1 of 1 |
Multistart 1 of 1 |
Multistart 1 of 1 |
Multistart 1 of 1 /
Multistart 1 of 1 |
Multistart 1 of 1 |
                   
res
#> Av. Lower CI Av. Lower Bound Av. Upper Bound Av. Upper CI
#>   0.02941        0.05817         0.93362       0.94874

# We can plot it with a generic plotting method
plot(res)
```

![](README-example-1.png)
