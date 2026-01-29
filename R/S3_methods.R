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

  # Create data for legend
  ci_data <- data.frame(
    observation = c(pdf$observation, pdf$observation),
    y = c(pdf$lower_ci, pdf$upper_ci),
    type = "95% CI"
  )

  ggplot2::ggplot(pdf, ggplot2::aes(x = observation)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper, color = "Bounds"),
                           alpha = 0.7) +
    ggplot2::geom_point(data = ci_data, ggplot2::aes(y = y, color = "95% CI"),
                        size = 0.5) +
    ggplot2::scale_color_manual(
      name = "",
      values = c("Bounds" = "steelblue", "95% CI" = "tomato"),
      guide = ggplot2::guide_legend(override.aes = list(
        linetype = c(1, 0),
        shape = c(NA, 16),
        size = c(0.5, 2)
      ))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::ggtitle("Partially Identified Conditional Expectation Estimates") +
    ggplot2::xlab("Observation (sorted by lower bound)") +
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

    # Create data for horizontal lines with legend
    hline_data <- data.frame(
      yintercept = c(0, x$ate_lower, x$ate_upper),
      line_type = c("Zero effect", "Average ATE bounds", "Average ATE bounds")
    )

    p <- ggplot2::ggplot(pdf, ggplot2::aes(x = observation)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = "CATE bounds"),
                           alpha = 0.4) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0, linetype = "Zero effect"),
                          color = "gray40") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = x$ate_lower, linetype = "Average ATE bounds"),
                          color = "tomato") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = x$ate_upper, linetype = "Average ATE bounds"),
                          color = "tomato", show.legend = FALSE) +
      ggplot2::scale_fill_manual(name = "", values = c("CATE bounds" = "steelblue")) +
      ggplot2::scale_linetype_manual(
        name = "",
        values = c("Zero effect" = "dashed", "Average ATE bounds" = "dotted")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom") +
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

    # Reshape for legend
    pdf_long <- data.frame(
      observation = rep(pdf$observation, 2),
      ymin = c(pdf$ey1_l, pdf$ey0_l),
      ymax = c(pdf$ey1_u, pdf$ey0_u),
      outcome = rep(c("E[Y(1)|X]", "E[Y(0)|X]"), each = nrow(pdf))
    )

    p <- ggplot2::ggplot(pdf_long, ggplot2::aes(x = observation)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ymin, ymax = ymax, fill = outcome),
                           alpha = 0.4) +
      ggplot2::scale_fill_manual(
        name = "Potential Outcome",
        values = c("E[Y(1)|X]" = "steelblue", "E[Y(0)|X]" = "tomato")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::ggtitle("Bounds on Potential Outcome Means") +
      ggplot2::xlab("Observation (sorted)") +
      ggplot2::ylab("E[Y(d)|X]")
  }
  p
}
