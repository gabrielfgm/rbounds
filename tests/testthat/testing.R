library(np)
library(tidyverse)

# fake data
N <- 1000
x <- matrix(rnorm(N*3), nrow = N)
y <- rbinom(N, 1, pnorm(rowSums(x)))
z <- rbinom(N, 1, .8)

fake_df <- data.frame(y, x, z)
fake_df$yz <- fake_df$y * fake_df$z

# Estimates
np_true <- npreg(y ~ x, data = fake_df)
np_low <- npreg(y ~ x, data = fake_df[fake_df$z==1,],
                newdata = fake_df)
np_z <- npreg(z ~ x, data = fake_df)

# results
res_df <- data.frame(true = np_true$mean,
                     low = np_low$mean*np_z$mean,
                     high = np_low$mean*np_z$mean + (1 - np_z$mean),
                     x)

plot_df <- gather(res_df, model, loc_mean, true, low, high)

# plotting
plot_df %>% ggplot(aes(x, loc_mean, group = model, color = model)) +
  geom_line()
