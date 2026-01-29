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
#' @param x A model fit by \code{pidoutcomes}
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#' @export
#'
#' @examples
#' \dontrun{
#' N <- 1000
#' x <- rnorm(N)
#' e <- rnorm(N)
#' y <- as.numeric(2*x + e > 0)
#' z <- rbinom(N, 1, .75)
#' y_obs <- z*y
#' df <- data.frame(y_obs, x, z)
#' m1 <- pidoutcomes(y_obs ~ x, z, df)
#' m1
#' }
#'
print.rbounds <- function(x, ...) {
  ave_stats <- lapply(x, function(v){round(mean(v), 5)})
  labs <- c("Av. Lower CI", "Av. Lower Bound", "Av. Upper Bound", "Av. Upper CI")
  spaces <- nchar(labs)
  to_print <- mapply(function(stat,space){pad(stat,space)}, ave_stats, spaces)
  cat("Av. Lower CI\tAv. Lower Bound\tAv. Upper Bound\tAv. Upper CI\n")
  cat(paste(to_print, collapse = "\t"))
  cat("\n")
  invisible(x)
}


#' A default plotting method for objects of class 'rbounds'
#'
#' @param x An object of class 'rbounds' estimated by \code{pidoutcomes}
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot2 plot. Can be modified with any standard ggplot2 options
#' @export
#'
#' @examples
#' \dontrun{
#' N <- 1000
#' x <- rnorm(N)
#' e <- rnorm(N)
#' y <- as.numeric(2*x + e > 0)
#' z <- rbinom(N, 1, .75)
#' y_obs <- z*y
#' df <- data.frame(y_obs, x, z)
#' m1 <- pidoutcomes(y_obs ~ x, z, df)
#' plot(m1)
#' }
#'
plot.rbounds <- function(x, ...) {
  pdf <- data.frame(lower_ci = x$lower_ci, lower = x$lower,
                    upper = x$upper, upper_ci = x$upper_ci)
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


################################
# Methods for rbounds_ate class #
################################

#' Print method for rbounds_ate objects
#'
#' @param x An object of class 'rbounds_ate' estimated by \code{ate_bounds}
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#' @export
#'
print.rbounds_ate <- function(x, ...) {
  cat("Manski Bounds on Average Treatment Effect\n")
  cat("==========================================\n\n")
  cat(sprintf("ATE bounds:     [%7.4f, %7.4f]\n", x$ate_lower, x$ate_upper))
  cat(sprintf("95%% CI:         [%7.4f, %7.4f]\n", x$ate_lower_ci, x$ate_upper_ci))
  cat(sprintf("\nOutcome range:  [%g, %g]\n", x$y_min, x$y_max))
  cat(sprintf("Observations:   %d\n", length(x$propensity)))
  cat(sprintf("Mean propensity: %.3f\n", mean(x$propensity)))
  invisible(x)
}


#' Plot method for rbounds_ate objects
#'
#' Creates a visualization of the conditional ATE bounds across observations.
#'
#' @param x An object of class 'rbounds_ate' estimated by \code{ate_bounds}
#' @param type Type of plot: "cate" for conditional bounds by observation (default),
#'   or "potential" to show bounds on potential outcome means
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot2 plot
#' @export
#'
plot.rbounds_ate <- function(x, type = c("cate", "potential"), ...) {
  type <- match.arg(type)

  if (type == "cate") {
    # Plot conditional ATE bounds
    pdf <- data.frame(
      lower = x$cate_lower,
      upper = x$cate_upper
    )
    pdf <- pdf[order(pdf$lower), ]
    pdf$observation <- seq_len(nrow(pdf))

    p <- ggplot2::ggplot(pdf, ggplot2::aes(x = observation, ymin = lower, ymax = upper)) +
      ggplot2::geom_ribbon(fill = "steelblue", alpha = 0.4) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      ggplot2::geom_hline(yintercept = x$ate_lower, linetype = "dotted", color = "tomato") +
      ggplot2::geom_hline(yintercept = x$ate_upper, linetype = "dotted", color = "tomato") +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle("Bounds on Conditional Average Treatment Effect") +
      ggplot2::xlab("Observation (sorted by lower bound)") +
      ggplot2::ylab("CATE Bounds")
  } else {
    # Plot bounds on potential outcomes
    pdf <- data.frame(
      ey1_l = x$ey1_lower,
      ey1_u = x$ey1_upper,
      ey0_l = x$ey0_lower,
      ey0_u = x$ey0_upper
    )
    pdf <- pdf[order(pdf$ey1_l), ]
    pdf$observation <- seq_len(nrow(pdf))

    p <- ggplot2::ggplot(pdf, ggplot2::aes(x = observation)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ey1_l, ymax = ey1_u),
                           fill = "steelblue", alpha = 0.4) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ey0_l, ymax = ey0_u),
                           fill = "tomato", alpha = 0.4) +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle("Bounds on Potential Outcome Means") +
      ggplot2::xlab("Observation (sorted)") +
      ggplot2::ylab("E[Y(d)|X]") +
      ggplot2::annotate("text", x = nrow(pdf) * 0.9, y = mean(x$ey1_upper),
                        label = "E[Y(1)|X]", color = "steelblue") +
      ggplot2::annotate("text", x = nrow(pdf) * 0.9, y = mean(x$ey0_lower),
                        label = "E[Y(0)|X]", color = "tomato")
  }
  p
}
