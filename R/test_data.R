################
# Testing Data #
################
# N <- 100
# x <- rnorm(N)
# e <- rnorm(N)
# y <- as.numeric(2*x + e > 0)
# z <- rbinom(N, 1, .75)
# y_obs <- z*y
# df <- data.frame(y_obs, x, z)
#
# m1 <- pidoutcomes(y_obs ~ x, z, df)
# plot(m1)
#
# # now try to force errors
# z_bad <- rnorm(N)
# y_bad <- rnorm(N)
#
# pidoutcomes(y_bad ~ x, z, df)
# pidoutcomes(y_obs ~ x, z_bad, df)
#
# df2 <- df
# df2$z_bad <- z_bad
#
# pidoutcomes(y_obs ~ x, z_bad, df2)
#
#
# # simple reprex
#
# fun1 <- function(var, frame) {
#   var_value <- eval(quote(var), envir = frame)
#   var_value
# }
#
# var <- 1:10
# var2 <- 11:20
#
# frame1 <- data.frame(var = var)
# frame2 <- data.frame(apple = var)
#
# # does what I expect
# fun1(var, frame1)
#
# # does what I don't expect
# fun1(var2, frame1)
#
# # does what I don't like
# fun1(var, frame2)
#
# # also what I don't like
# fun1(var2, frame2)
#
# # Does what I do not comprehend
# fun1(apple, frame2)
