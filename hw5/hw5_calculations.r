### hw5_calculations.r
### calculations for homework 5

# Problem 4
# Negative binomial distribution with r = 5, p = 0.32
r <- 5
p <- 0.32
q <- 1 - p
# (a) Mean and variance of W
mean_W <- (r*q) / p
var_W <- r * q / p^2
mean_W  # 10.625
var_W  # 33.203

# (b) P(W <= 10)
# Using the negative binomial pmf:
# P(W = w) = (r+w-1 choose r-1) p^r (1-p)^w,  w = 0, 1, 2, ...
# So,
w_vals <- 0:10
pmf_W <- choose(r + w_vals - 1, r - 1) * p^r * q^w_vals
p_W_leq_10 <- sum(pmf_W) # 0.552

# (c) P(Y >= 5), where Y ~ Binomial(n = 15, p = 0.32)
n <- 15
p_Y_geq_5 <- sum(dbinom(5:n, size = n, prob = p))
p_Y_geq_5  # 0.552

# Problem 6
# (b) lambda = 2.5, n = 5, t = 3, find P(Y >= 5) and P(T <= 3)
# for Y ~ Poisson(7.5) and T ~ gamma(5,0.4).
lambda <- 2.5
n <- 5
t <- 3
# (i) P(Y >= 5)
p_Y_geq_5 <- 1 - ppois(4, 7.5) # 0.868

# (ii) P(T <= 3)
p_T_leq_3 <- pgamma(t, shape = n, rate = lambda) # 0.868
