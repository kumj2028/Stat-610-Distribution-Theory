### problem3.r
### graphs of functions for problem 3
# install.packages("ggplot2")  # if needed
library(ggplot2)

## (a) F(t) = t(2 - t),  t in [0,1]
df1 <- data.frame(t = seq(0, 1, length.out = 1001))
df1$F <- df1$t * (2 - df1$t)
p1 <- ggplot(df1, aes(t, F)) +
  geom_line(linewidth = 1) +
  labs(title = "1) F(t) = t(2-t),  t in [0,1]", x = "t", y = "F(t)") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))

## (b) F(t) = t(2 - t),  t in [0,2]
df2 <- data.frame(t = seq(0, 2, length.out = 1001))
df2$F <- df2$t * (2 - df2$t)
p2 <- ggplot(df2, aes(t, F)) +
  geom_line(linewidth = 1) +
  labs(title = "2) F(t) = t(2-t),  t in [0,2]", x = "t", y = "F(t)") +
  coord_cartesian(xlim = c(0,2), ylim = c(0,1))

## (c) Piecewise:
##    F(t) = t(2−t) for t in [0,1/2)  and  F(t) = (t+7)/8 for t in [1/2,1]
df3a <- data.frame(t = seq(0, 0.5, length.out = 601))
df3a <- subset(df3a, t < 0.5)
df3a$F <- df3a$t * (2 - df3a$t)

df3b <- data.frame(t = seq(0.5, 1, length.out = 601))
df3b$F <- (df3b$t + 7)/8

p3 <- ggplot() +
  geom_line(data = df3a, aes(t, F), linewidth = 1) +
  geom_line(data = df3b, aes(t, F), linewidth = 1) +
  # open circle at left-limit (0.5−, 0.75)
  geom_point(aes(x = 0.5, y = 0.5*(2 - 0.5)), shape = 1, size = 3, stroke = 1.1) +
  # filled circle at right-value (0.5, 0.9375)
  geom_point(aes(x = 0.5, y = (0.5 + 7)/8), shape = 16, size = 2.8) +
  labs(title = "3) Piecewise on [0,1] with jump at t = 1/2", x = "t", y = "F(t)") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))

## (d) Step function on [0,1]:
##    F(t) = 1/5·1_{[0,inf)} + 1/4·1_{[1/2,inf)} 
##    + 1/2·1_{[3/4,inf)} + 1/20·1_{[1,inf)}
df4 <- data.frame(t = seq(0, 1, length.out = 1001))
ind <- function(x) as.numeric(x)  # explicit 0/1 casting
df4$F <- (1/5)*ind(df4$t >= 0) + (1/4)*ind(df4$t >= 0.5) +
          (1/2)*ind(df4$t >= 0.75) + (1/20)*ind(df4$t >= 1)

# Jump points (filled at right-continuous values; open at left-limits)
jumps_closed <- data.frame(t = c(0, 0.5, 0.75, 1.0),
                           F = c(0.20, 0.45, 0.95, 1.00))
jumps_open   <- data.frame(t = c(0.5, 0.75, 1.0),
                           F = c(0.20, 0.45, 0.95))

p4 <- ggplot(df4, aes(t, F)) +
  geom_step(direction = "hv", linewidth = 1) +
  geom_point(data = jumps_closed, aes(t, F), shape = 16, size = 2.8) +
  geom_point(data = jumps_open, aes(t, F), shape = 1, size = 3, stroke = 1.1) +
  labs(title = "4) Right-continuous step function on [0,1]", x = "t", y = "F(t)") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1.05))