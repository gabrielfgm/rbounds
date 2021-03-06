####################
# Printing Methods #
####################

pad <- function(s, n) {
  es <- n - nchar(s)
  if (es > 0) {
    bf <- paste(rep(" ", floor(es/2)), collapse = "")
    af <- paste(rep(" ", ceiling(es/2)), collapse = "")
    paste0(bf, s, af)
  }
}

#' A simple print method for rbounds objects
#'
#' @param m A model fit by \code{pidoutcomes}
#'
#' @return By default we print the mean upper and lower bounds and CI's
#' @export
#'
#' @examples
#' #' N <- 1000
#' x <- rnorm(N)
#' e <- rnorm(N)
#' y <- as.numeric(2*x + e > 0)
#' z <- rbinom(N, 1, .75)
#' y_obs <- z*y
#' df <- data.frame(y_obs, x, z)
#' m1 <- pidoutcomes(y_obs ~ x, z, df)
#' m1
#'
print.rbounds <- function(m) {
  ave_stats <- lapply(m, function(x){round(mean(x), 5)})
  labs <- c("Av. Lower CI", "Av. Lower Bound", "Av. Upper Bound", "Av. Upper CI")
  spaces <- nchar(labs)
  to_print <- mapply(function(stat,space){pad(stat,space)}, ave_stats, spaces)
  cat("Av. Lower CI\tAv. Lower Bound\tAv. Upper Bound\tAv. Upper CI\n")
  cat(paste(to_print, collapse = "\t"))
}


#' A default plotting method for objects of class 'rbounds'
#'
#' @param m An object of class 'rbounds' estimated by \code{pidoutcomes}
#'
#' @return A ggplot2 plot. Can be modified with any standard ggplot2 options
#' @export
#'
#' @examples
#' N <- 1000
#' x <- rnorm(N)
#' e <- rnorm(N)
#' y <- as.numeric(2*x + e > 0)
#' z <- rbinom(N, 1, .75)
#' y_obs <- z*y
#' df <- data.frame(y_obs, x, z)
#' m1 <- pidoutcomes(y_obs ~ x, z, df)
#' plot(m1)
#'
plot.rbounds <- function(m) {
  pdf <- data.frame(lower_ci = m$lower_ci, lower = m$lower,
                    upper = m$upper, upper_ci = m$upper_ci)
  pdf <- pdf[with(pdf, order(lower)), ]
  pdf$observation <- 1:nrow(pdf)

  ggplot2::ggplot(pdf,
                  ggplot2::aes(observation,
                               ymin = lower,
                               ymax = upper)) +
    ggplot2::geom_point(data = pdf, ggplot2::aes(x = observation,
                                                 y = lower_ci),
                        color = "tomato", size = .5) +
    ggplot2::geom_point(data = pdf, ggplot2::aes(x = observation,
                                                 y = upper_ci),
                        color = "tomato", size = .5) +
    ggplot2::geom_errorbar(color = "steelblue", alpha = .7) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle("Partially Identified Conditional Expectation Estimates") +
    ggplot2::xlab("Observation #") +
    ggplot2::ylab("Interval Estimate of E(y|x)")
}
