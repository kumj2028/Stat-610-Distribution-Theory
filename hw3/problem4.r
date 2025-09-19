### problem4.r
### graphs of pdf and cdf for problem 4
# install.packages("ggplot2")  # if needed
library(ggplot2)

## pdf f_V(v) = F_V'(v) = 2v 1_{[0,1]}(v)
df1 <- data.frame(v = seq(-0.5, 1.5, length.out = 1001))
df1$f <- ifelse(df1$v >= 0 & df1$v <= 1, 2*df1$v, 0)
p1 <- ggplot(df1, aes(v, f)) +
  geom_line(linewidth = 1) +
  labs(title = "pdf: f_V(v) = 2v for v in [0,1]", x = "v", y = "f_V(v)") +
  coord_cartesian(xlim = c(-0.5,1.5), ylim = c(0,2.2))

## cdf F_V(v) = v^2 1_{[0,1]}(v) + 0 \cdot 1_{(-\infty,0)}(v) +
## 1 \cdot 1_{(1,\infty)}(v).
df2 <- data.frame(v = seq(-0.5, 1.5, length.out = 1001))
ind <- function(x) as.numeric(x)  # explicit 0/1 casting
df2$F <- (df2$v^2)*ind(df2$v >= 0 & df2$v <= 1) + ind(df2$v > 1)
p2 <- ggplot(df2, aes(v, F)) +
  geom_step(direction = "hv", linewidth = 1) +
  labs(title = "cdf (right-continuous): F_V(v)", x = "v", y = "F_V(v)") +
  coord_cartesian(xlim = c(-0.5,1.5), ylim = c(-0.05,1.05))

