### problem6.r
### graphs of the pmf and cdf of a binomial distribution for problem 6
# install.packages("ggplot2")  # if needed
library(ggplot2)

n <- 6; p <- 17/100
df <- data.frame(
  y   = 0:n,
  pmf = dbinom(0:n, n, p),
  cdf = pbinom(0:n, n, p)
)
df$left <- c(0, head(df$cdf, -1))  # F(k-)

## PMF as spikes (vertical lines)
ggplot(df) +
  geom_segment(aes(x = y, xend = y, y = 0, yend = pmf), linewidth = 1.2) +
  geom_point(aes(x = y, y = pmf), size = 2) +
  scale_x_continuous(breaks = 0:n, limits = c(-0.5, n + 0.5)) +
  labs(title = sprintf("PMF (spikes): Binomial(%d, %.2f)", n, p),
       x = "y", y = "P(Y = y)")

## CDF: step with dark dots at jumps (and optional left limits)
ggplot(df, aes(x = y, y = cdf)) +
  geom_step(direction = "hv") +
  geom_point(size = 2) +                                   # F(k)
  geom_point(aes(y = left), shape = 21, fill = NA, size = 2) +  # F(k-)
  scale_x_continuous(breaks = 0:n, limits = c(-0.5, n + 0.5)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "CDF (right-continuous)",
       x = "y", y = "P(Y <= y)")

