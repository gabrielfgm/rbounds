# Functions for plotting the rbounds result

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

