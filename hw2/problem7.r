### problem7.r
### graphs of the pmf and cdf of a hypergeometric distribution for problem 7
# install.packages("ggplot2")  # if needed
library(ggplot2)

N <- 30; K <- 5; n <- 4
x <- 0:4
pmf <- choose(K, x) * choose(N - K, n - x) / choose(N, n)
cdf <- cumsum(pmf)

df <- data.frame(
  x   = x,
  pmf = pmf,
  cdf = cdf,
  left = c(0, head(cdf, -1))   # F(x-)
)

## PMF as vertical spikes (lines)
ggplot(df) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = pmf), linewidth = 1.2) +
  geom_point(aes(x = x, y = pmf), size = 2) +
  scale_x_continuous(breaks = x, limits = c(-0.5, 4.5)) +
  labs(title = sprintf("PMF (spikes): Hypergeom(N=%d, K=%d, n=%d)", N, K, n),
       x = "x", y = "P(X = x)")

## CDF: right-continuous step with darkened jump dots
ggplot(df, aes(x = x, y = cdf)) +
  geom_step(direction = "hv") +
  geom_point(size = 2) +                               # filled dots at F(x)
  geom_point(aes(y = left), shape = 21, fill = NA,     # optional left limits
             stroke = 0.8, size = 2) +
  scale_x_continuous(breaks = x, limits = c(-0.5, 4.5)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "CDF (right-continuous)",
       x = "x", y = "P(X <= x)")
