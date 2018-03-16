# This is a helper function to swap the dependent variable in a formula object
dep_var_switcher <- function(form, swap) {
  reformulate(deparse(form[[3]]), response = swap)
}


# This is the model that estimates lower and upper bounds with missing outcomes
pidoutcomes <- function(outformula, # Formula for the conditional outcome
                        z, # name of variable indicating missing/non-missing outcomes
                        data, # The data frame
                        ...) {
  # Missing data vector
  Z <- data[, z]

  # estimate conditional density of outcome for known cases
  y.model <- np::npcdens(outformula, data=data[z==1,], newdata = data)
  y.probs_at_x <- fitted(y.model)

  # Estimate the conditional probability of observation
  z.model <- np::npcdens(dep_var_switcher(outformula, z), data=data, newdata=data)
  z.probs_at_x <- fitted(z.model)

  # Compute worst case bounds
  m_l <- y.probs_at_x * z.probs_at_x
  m_u <- m_l + (1 - z.probs_at_x)
}

# bivariate worst case bounds for binary outcomes
wcb <- function(y, z) {
  m_l <- mean(y[z==1]) * mean(z)
  m_u <- m_l + (1 - mean(z))
  c('lower' = m_l, 'upper' = m_u)
}

# miv wcb
miv_wcb <- function(y, z, v) {
  levs <- unique(v)
  res <- do.call(rbind, lapply(levs, function(l){wcb(y[v==l], z[v==l])}))
  res <- data.frame(levs, res)
  for (l in res$levs) {
    res$lower[res$levs==l] <- max(res$lower[res$levs <=l])
    res$upper[res$levs==l] <- min(res$upper[res$levs >=l])
  }
  res
}

miv_wcb(y, z, v)


