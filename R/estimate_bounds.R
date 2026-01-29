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
  np_lower_bw <- suppressMessages(np::npregbw(outformula, data = sm.data, ...))
  np_lower <- suppressMessages(np::npreg(np_lower_bw, newdata = data))

  # Estimate the conditional probability of observation: P(Z=1|X)
  z_form <- dep_var_switcher(outformula, z_name)
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
                    N = N, alpha = alpha)
  }))

  # Return bounds
  res <- list(lower_ci = unlist(conf_ints[,1]),
       lower = m_l,
       upper = m_u,
       upper_ci = unlist(conf_ints[,2]))
  class(res) <- "rbounds"
  res
}

