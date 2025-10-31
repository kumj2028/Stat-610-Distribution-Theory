### calculations for homework 8

# Problem 5 plot y=g(x) versus x and y versus x=h(y) on the same graph
g <- function(x) {
  return((3*x + 4)/(6*(x + 1)))
}
h <- function(y) {
  return((8 + 12*y)/(3*(1 + 4*y)))
}

x <- seq(0, 5, by=0.01)
y <- seq(0, 5, by=0.01)

plot(x, g(x), type='l', col='blue', ylim=c(0,5), ylab='y', xlab='x')
lines(h(y), y, col='red')
legend("topright", legend=c("y=g(x)", "x=h(y)"), col=c("blue", "red"), lty=1)