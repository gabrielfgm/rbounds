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
  # tmpfun <- get("explodeFormula", envir = asNamespace("np"))
  # environment(explodeFormula) <- environment(tmpfun)
  # assignInNamespace("explodeFormula", explodeFormula, ns = "np")

  # estimate conditional density of outcome for known cases
  sm.data <- data[data[,z]==1,]
  require(locfit)
  y.model <- locfit::locfit(outformula,
                          data=na.omit(sm.data),
                          family = "binomial", link="logit",
                          maxk = 1000)
  y.probs_at_x <- predict(y.model, newdata=data)

  # Estimate the conditional probability of observation
  z.model <- locfit::locfit(dep_var_switcher(outformula, z),
                         data=data, family="binomial",
                         link = 'logit', maxk = 1000)
  z.probs_at_x <- predict(z.model, newdata=data)

  # Compute worst case bounds
  m_l <- y.probs_at_x * z.probs_at_x
  m_u <- m_l + (1-z.probs_at_x)

  # Return bounds
  data.frame(lower = m_l, upper = m_u)
}

plot.bounds <- function(res, c=.5, type = "count") {
  res$decision <- "Undecidable"
  res$decision[res$lower > c] <- "Fraudulent"
  res$decision[res$upper < c] <- "Not Fraudulent"
  require(ggplot2)
  require(ggalt)
  if (type=="count") {
  df1 <- as.data.frame(table(res$decision))
  names(df1) <- c("decision", "Count")
  ggplot(df1, aes(decision, Count/sum(Count),
                  label = Count)) +
    geom_lollipop(point.colour="coral", point.size=4) +
    geom_text(vjust=-1) + ylim(c(0,1)) +
    theme_minimal()
  } else {
    res$decision <- factor(res$decision,
                           levels = c("Not Fraudulent", "Undecidable",
                                      "Fraudulent"))
    ggplot(res[order(res$decision, res$upper, res$lower),],
           aes(x=lower, xend=upper, y= 1:nrow(res),
               color = decision)) +
      geom_dumbbell(size=.5, alpha=.3,
             colour_x = 'black', colour_xend='black',
             size_x = 1, size_xend = 1) +
      theme_minimal() + ylab("Railway Companies") +
      xlab("Conditional Probability of Fraud") +
      geom_vline(xintercept = c, color='black') +
      coord_flip() +
      scale_color_manual(values = c('gold', 'black', 'red'))
  }
}

