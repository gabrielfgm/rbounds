# This is a helper function to swap the dependent variable in a formula object
dep_var_switcher <- function(form, swap) {
  reformulate(deparse(form[[3]]), response = swap)
}

# We need to fix a bug in a helper function within np, so this will replace
#  the np function explodeFormula
explodeFormula <- function(formula) {
  res <- strsplit(strsplit(paste(deparse(eval(formula)), collapse=""), " *[~] *")[[1]], " *[+] *")
  stopifnot(all(sapply(res,length) > 0))
  names(res) <- c("response","terms")
  res
}


# This is the model that estimates lower and upper bounds with missing outcomes
pidoutcomes <- function(outformula, # Formula for the conditional outcome
                        z, # name of variable indicating missing/non-missing outcomes
                        data, # The data frame
                        ...) {

  # Ensure formula is properly formatted
  outformula <- as.formula(outformula)

  # now ensure selection indicator is a factor
  # data[, z] <- factor(data[, z], levels = c("1", "0"))

  # now fix namespace so it reads formulas
  tmpfun <- get("explodeFormula", envir = asNamespace("np"))
  environment(explodeFormula) <- environment(tmpfun)
  assignInNamespace("explodeFormula", explodeFormula, ns = "np")

  # estimate conditional density of outcome for known cases
  #sm.data <- data[data[,z]==1,]
  np_lower_bw <- np::npregbw(outformula,
                             data=data[data[, z]==1, ])
  np_lower <- np::npreg(np_lower_bw,
                          newdata = data)

  # Estimate the conditional probability of observation
  np_missing_bw <- np::npregbw(dep_var_switcher(outformula, z),
                               data=data)
  np_missing <- np::npreg(np_missing_bw)

  # Compute worst case bounds
  m_l <- np_lower$mean * np_missing$mean
  m_u <- m_l + (1-np_missing$mean)
  temp.df <- data.frame(m_l, m_u)

  # get confidence intervals
  sigma <- sqrt(sum(residuals(np_lower)^2)/(sum(data[,z]) - 1))
  conf_ints <- do.call(rbind, apply(temp.df, 1, function(row){
    conf_int_bounds(row[2], row[1], sigma, sigma, nrow(data))
  }))

  # Return bounds
  list(lower_ci = unlist(conf_ints[,1]),
       lower = m_l,
       upper = m_u,
       upper_ci = unlist(conf_ints[,2]),
       class = "rbounds")
}

