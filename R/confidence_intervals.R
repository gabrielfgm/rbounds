########################
# Confidence Intervals #
########################

# This code computes confidence intervals for the location of a parameter within
# an identified set. It is based on Imbens and Manski (2004).

conf_int_bounds <- function(par_u, par_l, sigma_u, sigma_l, N, alpha = .95) {
  delta = par_u - par_l
  max_sig <- max(sigma_l, sigma_u)
  c_consts <- get_c_consts(N, delta, max_sig, alpha)
  lower <- par_l - (c_consts * sigma_l/sqrt(N))
  upper <- par_u + (c_consts * sigma_u/sqrt(N))
  res <- list(lower, upper)
  names(res) <- paste(c("Lower", "Upper"), paste0(100*alpha, "% CI"), sep = " ")
  res
}

get_c_consts <- function(N, delta, max_sig, alpha) {
  opt_fun <- function(c_consts) {
    log(abs((pnorm(c_consts + sqrt(N)*delta/max_sig) - pnorm(-c_consts)) - alpha))
  }
  res <- optimise(opt_fun, c(0,5))
  res$minimum
}

