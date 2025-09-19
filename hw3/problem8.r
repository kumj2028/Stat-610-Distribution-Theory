### problem4.r
### graphs of pdf and cdf for problem 4
# install.packages("ggplot2")  # if needed
library(ggplot2)

## pdf f_X(x) = 4xe^{-2x} for x > 0, and f_X(x) = 0 otherwise
df1 <- data.frame(x = seq(-0.5, 5, length.out = 1001))
df1$f <- ifelse(df1$x > 0, 4*df1$x*exp(-2*df1$x), 0)
p1 <- ggplot(df1, aes(x, f)) +
  geom_line(linewidth = 1) +
  labs(title = "pdf: f_X(x) = 4xe^{-2x} for x > 0", x = "x", y = "f_X(x)") +
  coord_cartesian(xlim = c(-0.5,5), ylim = c(0,1.1))

## cdf F_X(x) = 1 - e^{-2x} - 2xe^{-2x} for x > 0, and F_X(x) = 0 for x <= 0
df2 <- data.frame(x = seq(-0.5, 5, length.out = 1001))
df2$F <- ifelse(df2$x > 0, 1 - exp(-2*df2$x) - 2*df2$x*exp(-2*df2$x), 0)
p2 <- ggplot(df2, aes(x, F)) +
  geom_line(linewidth = 1) +
  labs(title = "cdf: F_X(x) = 1 - e^{-2x} - 2xe^{-2x} for x > 0", x = "x", y = "F_X(x)") +
  coord_cartesian(xlim = c(-0.5,5), ylim = c(0,1.1))
