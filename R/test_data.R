################
# Testing Data #
################
N <- 1000
x <- rnorm(N)
e <- rnorm(N)
y <- as.numeric(2*x + e > 0)
z <- rbinom(N, 1, .75)

y_obs <- z*y

df <- data.frame(y_obs, x, z)

# get output
m1 <- pidoutcomes(y_obs~x, "z", data = df)
