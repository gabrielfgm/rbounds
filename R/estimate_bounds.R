#' @importFrom stats model.frame as.formula reformulate pnorm optimise
#' @importFrom utils globalVariables
NULL

# Suppress R CMD check notes about ggplot2 non-standard evaluation
utils::globalVariables(c("observation", "lower", "upper", "lower_ci", "upper_ci",
                         "ey1_l", "ey1_u", "ey0_l", "ey0_u"))

# This is a helper function to swap the dependent variable in a formula object
dep_var_switcher <- function(form, swap) {
  reformulate(deparse(form[[3]]), response = swap)
}

# This is the model that estimates lower and upper bounds
# with missing outcomes



#' Partial Identification of Conditional Expectations with Missing Outcomes
#'
#' \code{pidoutcomes} computes the upper and lower bounds on the
#' conditional expectation function when there are missing outcomes.
#' It only works for a dependent variable scaled such that
#' \eqn{y \in (0,1)}.
#' It is based on the work of Manski (2007). Assume we want to estimate
#' \deqn{P(y|x) = P(y|x,z=1)P(z=1|x) + P(y|x,z=0)P(z=0|x),}
#' where \code{z} is a binary variable indicating missingness and
#' \code{y} is a binary outcome.
#' All of the quantities are identifiable from the data apart from
#' \eqn{P(y|x,z=0)} which is by definition unobservable. However,
#' this missing quantity cannot be less than zero or greater than
#' one so the absolute bounds on the conditional probability are
#' \deqn{P(y|x,z=1)P(z=1|x) \le P(y|x) \le P(y|x,z=1)P(z=1|x) + P(z=0|x).}
#' \code{pidoutcomes()} computes these upper and lower bounds.
#'
#' We also compute confidence intervals based on Manski and Imbens (2004).
#' We focus on intervals that are guaranteed to cover the parameter
#' at the specified level, rather than intervals that are guaranteed to
#' cover the bounds at the specified level.
#'
#' @param outformula A formula
#' @param z A column from your data that indicates missing outcomes, must be 0 or 1
#' @param data Your data
#' @param alpha The alpha significance level for the CI. Default is \code{alpha=.95}
#' @param ... Other arguments accepted by \code{npreg}
#'
#' @return The function returns a list of 4 values: \code{lower_ci},
#' \code{lower}, \code{upper} and \code{upper_ci}, each a vector of
#' length \code{nrow(data)}. These are the lower and upper confidence
#' intervals on the location
#' of the parameter (as described in Manski and Imbens (2004)), and the
#' estimated lower and upper bounds of the partially identified conditional
#' mean.
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
#'
pidoutcomes <- function(outformula, # Formula for the conditional outcome
                        z, # name of variable indicating missing/non-missing outcomes
                        data, # The data frame
                        alpha = .95,
                        ...) {

  # Get the indicator variable
  z_name <- deparse(substitute(z))
  # Check for z in df
  if (!(z_name %in% names(data))) {stop("The variable z must appear in data.")}
  z_col <- data[[z_name]]

  # Check that z and y are actually binary
  y <- model.frame(outformula, data = data)[[1]]
  if (!(min(y) == 0 & max(y) == 1)) {
    stop("The dependent variable must be binary.", call. = FALSE)
  }
  if (!(min(z_col) == 0 & max(z_col) == 1)) {
    stop("The missing outcomes indicator z variable must be binary.", call. = FALSE)
  }

  # Estimate conditional mean of outcome for observed cases: E[Y|X, Z=1]
  sm.data <- data[z_col == 1, ]
  # Create formula in current environment to avoid scoping issues with np
  outcome_form <- as.formula(deparse(outformula), env = environment())
  np_lower_bw <- suppressMessages(np::npregbw(outcome_form, data = sm.data, ...))
  np_lower <- suppressMessages(np::npreg(np_lower_bw, newdata = data))

  # Estimate the conditional probability of observation: P(Z=1|X)
  z_form <- as.formula(deparse(dep_var_switcher(outformula, z_name)), env = environment())
  np_missing_bw <- suppressMessages(np::npregbw(z_form, data = data))
  np_missing <- suppressMessages(np::npreg(np_missing_bw))

  # extract values to clarify algebra
  p <- np_missing$mean
  mu <- np_lower$mean
  not_p <- (1 - p)

  # Get standard errors from np models
  se_mu <- np::se(np_lower)      # SE of E[Y|X, Z=1]
  se_p <- np::se(np_missing)     # SE of P(Z=1|X)

  # Compute worst case bounds
  # Lower: E_L = mu * p
  # Upper: E_U = mu * p + (1 - p)
  m_l <- mu * p
  m_u <- m_l + not_p

  # Variance of bounds using delta method (treating mu and p as independent)
  # Var(E_L) = Var(mu * p) approx= p^2 * Var(mu) + mu^2 * Var(p)
  # Var(E_U) = Var(mu * p + 1 - p) = Var((mu - 1) * p + 1) approx= (mu-1)^2 * Var(p) + p^2 * Var(mu)
  var_l <- p^2 * se_mu^2 + mu^2 * se_p^2
  var_u <- p^2 * se_mu^2 + (mu - 1)^2 * se_p^2

  # Convert to standard deviations for conf_int_bounds
  # Use pmax to avoid numerical issues with very small variances
  sd_l <- sqrt(pmax(var_l, 0))
  sd_u <- sqrt(pmax(var_u, 0))

  temp.df <- data.frame(m_l, m_u, sigma_l = sd_l, sigma_u = sd_u)

  N <- nrow(data)
  conf_ints <- do.call(rbind, apply(temp.df, 1, function(row){
    conf_int_bounds(row[2], row[1], sigma_u = row[4], sigma_l = row[3],
                    N = N, alpha = alpha, clip_lower = 0, clip_upper = 1)
  }))

  # Return bounds
  res <- list(lower_ci = unlist(conf_ints[,1]),
       lower = m_l,
       upper = m_u,
       upper_ci = unlist(conf_ints[,2]))
  class(res) <- "rbounds"
  res
}


#' Bounds on Average Treatment Effects (ATE)
#'
#' \code{ate_bounds} computes Manski worst-case bounds on the Average Treatment
#' Effect when treatment assignment is non-random (selection on unobservables).
#'
#' The ATE is defined as \eqn{\tau = E[Y(1)] - E[Y(0)]} where \eqn{Y(1)} and
#' \eqn{Y(0)} are potential outcomes under treatment and control. Since we only
#' observe \eqn{Y(1)} for treated units and \eqn{Y(0)} for control units, the
#' counterfactual outcomes are not identified without assumptions.
#'
#' Following Manski (1990, 2003), if outcomes are bounded (\eqn{y \in [y_{min}, y_{max}]}),
#' we can bound the potential outcome means and thus the ATE:
#' \deqn{\tau^L = E^L[Y(1)] - E^U[Y(0)]}
#' \deqn{\tau^U = E^U[Y(1)] - E^L[Y(0)]}
#'
#' @param formula A formula of the form \code{outcome ~ covariates}
#' @param treatment Name of the binary treatment variable (unquoted)
#' @param data A data frame containing the variables
#' @param y_min Minimum possible value of the outcome (default 0)
#' @param y_max Maximum possible value of the outcome (default 1)
#' @param alpha Confidence level for intervals (default 0.95)
#' @param ... Additional arguments passed to \code{np::npregbw}
#'
#' @return An object of class \code{rbounds_ate} containing:
#' \itemize{
#'   \item \code{ate_lower}: Average lower bound on ATE
#'   \item \code{ate_upper}: Average upper bound on ATE
#'   \item \code{ate_lower_ci}: Lower confidence interval endpoint for ATE
#'   \item \code{ate_upper_ci}: Upper confidence interval endpoint for ATE
#'   \item \code{cate_lower}: Conditional ATE lower bounds (one per observation)
#'   \item \code{cate_upper}: Conditional ATE upper bounds (one per observation)
#'   \item \code{ey1_lower}, \code{ey1_upper}: Bounds on E[Y(1)|X]
#'   \item \code{ey0_lower}, \code{ey0_upper}: Bounds on E[Y(0)|X]
#'   \item \code{propensity}: Estimated P(D=1|X)
#' }
#'
#' @references
#' Manski, C.F. (1990). "Nonparametric Bounds on Treatment Effects."
#' \emph{American Economic Review Papers and Proceedings}, 80(2), 319-323.
#'
#' Manski, C.F. (2003). \emph{Partial Identification of Probability Distributions}.
#' Springer.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simulate data with selection into treatment
#' set.seed(123)
#' N <- 500
#' x <- rnorm(N)
#' # Treatment assignment depends on x (selection)
#' d <- rbinom(N, 1, pnorm(0.5 * x))
#' # Potential outcomes
#' y0 <- rbinom(N, 1, pnorm(-0.5 + 0.3 * x))
#' y1 <- rbinom(N, 1, pnorm(0.5 + 0.3 * x))
#' # Observed outcome
#' y <- d * y1 + (1 - d) * y0
#' df <- data.frame(y = y, x = x, d = d)
#'
#' # Estimate ATE bounds
#' res <- ate_bounds(y ~ x, treatment = d, data = df)
#' res
#' }
#'
ate_bounds <- function(formula,
                       treatment,
                       data,
                       y_min = 0,
                       y_max = 1,
                       alpha = 0.95,
                       ...) {

  # Get treatment variable name and column

  d_name <- deparse(substitute(treatment))
  if (!(d_name %in% names(data))) {
    stop("The treatment variable must appear in data.", call. = FALSE)
  }
  d_col <- data[[d_name]]

  # Validate inputs
  y <- model.frame(formula, data = data)[[1]]
  if (min(y) < y_min || max(y) > y_max) {
    stop(sprintf("Outcome values must be in [%g, %g].", y_min, y_max), call. = FALSE)
  }
  if (!all(d_col %in% c(0, 1))) {
    stop("The treatment variable must be binary (0 or 1).", call. = FALSE)
  }
  if (sum(d_col == 1) == 0 || sum(d_col == 0) == 0) {
    stop("Both treatment and control groups must have observations.", call. = FALSE)
  }

  # Split data by treatment status
  data_treated <- data[d_col == 1, ]
  data_control <- data[d_col == 0, ]

  # Create formula in current environment to avoid scoping issues with np
  outcome_form <- as.formula(deparse(formula), env = environment())

  # Estimate E[Y|D=1, X] - conditional mean for treated
  np_treated_bw <- suppressMessages(np::npregbw(outcome_form, data = data_treated, ...))
  np_treated <- suppressMessages(np::npreg(np_treated_bw, newdata = data))
  ey_d1 <- np_treated$mean
  se_ey_d1 <- np::se(np_treated)

  # Estimate E[Y|D=0, X] - conditional mean for control
  np_control_bw <- suppressMessages(np::npregbw(outcome_form, data = data_control, ...))
  np_control <- suppressMessages(np::npreg(np_control_bw, newdata = data))
  ey_d0 <- np_control$mean
  se_ey_d0 <- np::se(np_control)

  # Estimate P(D=1|X) - propensity score
  ps_form <- as.formula(deparse(dep_var_switcher(formula, d_name)), env = environment())
  np_ps_bw <- suppressMessages(np::npregbw(ps_form, data = data))
  np_ps <- suppressMessages(np::npreg(np_ps_bw))
  p_d1 <- np_ps$mean
  p_d0 <- 1 - p_d1
  se_ps <- np::se(np_ps)

  # Compute bounds on E[Y(1)]
  # E^L[Y(1)] = E[Y|D=1,X] * P(D=1|X) + y_min * P(D=0|X)
  # E^U[Y(1)] = E[Y|D=1,X] * P(D=1|X) + y_max * P(D=0|X)
  ey1_lower <- ey_d1 * p_d1 + y_min * p_d0
  ey1_upper <- ey_d1 * p_d1 + y_max * p_d0

  # Compute bounds on E[Y(0)]
  # E^L[Y(0)] = y_min * P(D=1|X) + E[Y|D=0,X] * P(D=0|X)
  # E^U[Y(0)] = y_max * P(D=1|X) + E[Y|D=0,X] * P(D=0|X)
  ey0_lower <- y_min * p_d1 + ey_d0 * p_d0
  ey0_upper <- y_max * p_d1 + ey_d0 * p_d0

  # Compute bounds on ATE = E[Y(1)] - E[Y(0)]
  # tau^L = E^L[Y(1)] - E^U[Y(0)]
  # tau^U = E^U[Y(1)] - E^L[Y(0)]
  cate_lower <- ey1_lower - ey0_upper
  cate_upper <- ey1_upper - ey0_lower

  # Average bounds (integrate over X distribution)
  ate_lower <- mean(cate_lower)
  ate_upper <- mean(cate_upper)

  # Variance estimation for ATE bounds using delta method
  # For tau^L = ey1_lower - ey0_upper:
  #   ey1_lower = ey_d1 * p_d1 + y_min * (1 - p_d1)
  #   ey0_upper = y_max * p_d1 + ey_d0 * (1 - p_d1)
  # Simplified variance (assuming independence):
  var_ey1_l <- p_d1^2 * se_ey_d1^2 + (ey_d1 - y_min)^2 * se_ps^2
  var_ey1_u <- p_d1^2 * se_ey_d1^2 + (ey_d1 - y_max)^2 * se_ps^2
  var_ey0_l <- p_d0^2 * se_ey_d0^2 + (y_min - ey_d0)^2 * se_ps^2
  var_ey0_u <- p_d0^2 * se_ey_d0^2 + (y_max - ey_d0)^2 * se_ps^2

  # Variance of ATE bounds (treating ey1 and ey0 bounds as independent)
  var_tau_l <- var_ey1_l + var_ey0_u
  var_tau_u <- var_ey1_u + var_ey0_l

  sd_tau_l <- sqrt(pmax(mean(var_tau_l), 0))
  sd_tau_u <- sqrt(pmax(mean(var_tau_u), 0))

  # Compute confidence interval for average ATE
  N <- nrow(data)
  ate_ci <- conf_int_bounds(
    par_u = ate_upper,
    par_l = ate_lower,
    sigma_u = sd_tau_u,
    sigma_l = sd_tau_l,
    N = N,
    alpha = alpha
  )

  # Return results
  res <- list(
    ate_lower = ate_lower,
    ate_upper = ate_upper,
    ate_lower_ci = ate_ci[[1]],
    ate_upper_ci = ate_ci[[2]],
    cate_lower = cate_lower,
    cate_upper = cate_upper,
    ey1_lower = ey1_lower,
    ey1_upper = ey1_upper,
    ey0_lower = ey0_lower,
    ey0_upper = ey0_upper,
    propensity = p_d1,
    y_min = y_min,
    y_max = y_max
  )
  class(res) <- "rbounds_ate"
  res
}

