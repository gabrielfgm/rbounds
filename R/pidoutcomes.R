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

  # Missing data vector
  Z <- data[, z]

  # now ensure selection indicator is a factor
  data[, z] <- as.factor(data[, z])

  # now fix namespace so it reads formulas
  tmpfun <- get("explodeFormula", envir = asNamespace("np"))
  environment(explodeFormula) <- environment(tmpfun)
  assignInNamespace("explodeFormula", explodeFormula, ns = "np")

  # estimate conditional density of outcome for known cases
  sm.data <- data[z==1,]
  sub.bw <- np::npcdensbw(outformula,
                          data=sm.data)
  y.model <- np::npcdens(sub.bw, newdata = data)
  y.probs_at_x <- y.model$condens

  # Estimate the conditional probability of observation
  z.model <- np::npcdens(dep_var_switcher(outformula, z), data=data, newdata=data)
  z.probs_at_x <- z.model$condens

  # Compute worst case bounds
  m_l <- y.probs_at_x * z.probs_at_x
  m_u <- m_l + (1 - z.probs_at_x)

  # Return bounds
  list("lower" = m_l, "upper" = m_u)
}

####### Testing data
N <- 100
z <- rbinom(N, 1, .8)
x_cont <- rnorm(N)
x_dis <- rbinom(N, 1, .5)

# instrument for later
v <- rep(c(1,2,3,4), each=N/4)

# errors
e <- rnorm(N)

# depvar: nonliner function
y <- as.numeric((pnorm(x_cont) + .5 * x_dis + .2 * v + e) > 0)

df <- data.frame(y, z, x_cont, x_dis, v)

## Trial:
res <- pidoutcomes(outformula =  as.factor(y) ~ x_cont + as.factor(x_dis), z ="z", data = df)

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


