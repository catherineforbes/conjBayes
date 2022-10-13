# Beta distribution

u <- 3
v <- 10

bb <- seq(0.001, 0.999, 0.001)
plot(bb, dbeta(bb, u, v), type="l")
abline(v=u/(u+v))

betamean <- u/(u+v)


# Conditional Binomial distribution

p <- betamean # fix p

n <- 20

probs <- rep(NA,n+1)

for(i in 0:(n-1)){
  probs[(i+1)] <- dbinom(i,size=n, prob = p)
}


plot(0:20, probs, type="h", col="blue")
par(new=TRUE)

p <- 0.5

for(i in 0:(n-1)){
  probs[(i+1)] <- dbinom(i,size=n, prob = p)
}

points(0:20, probs, type="h", col="green")


## Poisson Gamma

# Gamma prior with alpha=3, beta = 1
alpha <- 3
beta <- 1

lambdagrid <- seq(0.001, 20, 0.01)
plambda <- dgamma(lambdagrid, shape = alpha, rate=beta)
plot(lambdagrid, plambda, type="l",ylim=c(0,4))

# Data information


y <- c(4, 5, 4, 1, 0, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6, 3, 3,
       5, 4, 5, 3, 1, 4, 4, 1, 5, 5, 3, 4, 2, 5, 2, 2, 3, 4, 2,
       1, 3, 2, 2, 1, 1, 1, 1, 3, 0, 0, 1, 0, 1, 1, 0, 0, 3, 1,
       0, 3, 2, 2, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 2, 1, 0, 0,
       0, 1, 1, 0, 2, 3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 4, 2, 0, 0,
       0, 1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1)

n <- length(y)

# The posterior distribution is Gamma(alpha + sum(y), beta + n) See slide 33

posterior_alpha <- alpha + sum(y)
posterior_beta <- beta + n

par(new=TRUE)
ppostlambda <- dgamma(lambdagrid, shape = posterior_alpha, rate= posterior_beta)
lines(lambdagrid, ppostlambda,  col="blue")
abline(v=mean(y))
