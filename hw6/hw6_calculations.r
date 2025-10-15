### hw5_calculations.r
### calculations for homework 5

# Problem 2 plot the hazard functions
# (a) h(x) = x/(beta(x+beta)) for beta = 1,2,5
beta_vals <- c(1, 2, 5)
x <- seq(0, 10, by = 0.1)
h <- function(x, beta) {
  return(x / (beta * (x + beta)))
}
plot(x, h(x, beta_vals[1]), type = "l", col = "blue", lwd = 2,
     ylab = "h(x)", xlab = "x",
     main = "Hazard functions for different beta values")
lines(x, h(x, beta_vals[2]), col = "red", lwd = 2)
lines(x, h(x, beta_vals[3]), col = "green", lwd = 2)
legend("topright", legend = paste("beta =", beta_vals), col = 1:3, lty = 1)

# (b) h(x) = (1 + 4e^(-x))/(1 + 2e^(-x)).
h2 <- function(x) {
  return((1 + 4 * exp(-x)) / (1 + 2 * exp(-x)))
}
plot(x, h2(x), type = "l", col = "purple", lwd = 2,
     ylab = "h(x)", xlab = "x",
     main = "Hazard function for mixture of exponentials distribution")

# Problem 7 graph of curved parameter space
a <- 1
theta <- seq(-10, 10, by=0.1)
w1 <- 1/(a*theta)
w2 <- -1/(2*a*theta^2)
plot(w1, w2, type='l', xlab='w1', ylab='w2', main='Curved Parameter Space')