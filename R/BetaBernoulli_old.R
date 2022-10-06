#' Calculates conjugate Beta posterior from Beta prior and Bernoulli data
#'
#' @param y A vector of data, can be empty.
#' @param a0 A number representing the Beta prior first shape parameter.
#' @param b0 A number representing the Beta prior second shape parameter.
#' @param pr A vector of probabilities to report posterior quantiles.
#' @return The sum of `x` and `y`.
#' @examples
#' BetaBernoulli(y = numeric(), a0 = 0.5, b0 = 0.5, plot = TRUE)
#' BetaBernoulli(y = c(1,1,1,0,0), a0 = 0.5, b0 = 0.5, plot = TRUE)

BetaBernoulli <- function(y = numeric(), a0 = 0.5, b0 = 0.5,
                            pr = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)){

  # would be good to make input values more robust

  # test
#  y = numeric()
#  a0 = 0.5
#  b0 = 0.5
#  pr = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)
  ######

  # if(is.na(y) == TRUE){
  #   y <- numeric()
  # }

  n <- length(y)

  sumy <- sum(y)
  a1 <- a0 + sumy # update first hyperparameter (shape 1)
  b1 <- b0 + n - sumy # update second hyperparameter (shape 2)

  ############ we have found the posterior distribution above ############
  ############ now we want to output various properties of it ############

  # posterior (updated) Beta distribution mean and variance
  m <- a1/(a1 + b1) # mean
  v <- a1*b1/(((a1 + b1)^2)*(a1 + b1 + 1)) # variance

  # posterior (updated) Beta distribution quantiles
  nn <- length(pr)
  q <- rep(NA, nn)
  for(i in 1:nn){
    q[i] <- qbeta(pr[i], shape1 = a1, shape2 = b1)
  }

  out <- list(a1 = a1, b1 = b1, m = m, v = v, pr = pr, q = q)

  return(out)

}
