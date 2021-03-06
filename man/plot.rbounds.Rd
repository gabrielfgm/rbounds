% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidence_intervals.R
\name{plot.rbounds}
\alias{plot.rbounds}
\title{A default plotting method for objects of class 'rbounds'}
\usage{
\method{plot}{rbounds}(m)
}
\arguments{
\item{m}{An object of class 'rbounds' estimated by \code{pidoutcomes}}
}
\value{
A ggplot2 plot. Can be modified with any standard ggplot2 options
}
\description{
A default plotting method for objects of class 'rbounds'
}
\examples{
N <- 1000
x <- rnorm(N)
e <- rnorm(N)
y <- as.numeric(2*x + e > 0)
z <- rbinom(N, 1, .75)
y_obs <- z*y
df <- data.frame(y_obs, x, z)
m1 <- pidoutcomes(y_obs ~ x, z, df)
plot(m1)

}
