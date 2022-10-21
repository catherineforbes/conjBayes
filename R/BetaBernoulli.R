#' Calculates conjugate Beta posterior from Beta prior and Bernoulli data
#'
#' @param y A vector of binary (0 or 1) data, can be empty.
#' @param a0 A positive number, the Beta prior first shape parameter.
#' @param b0 A postive number, the Beta prior second shape parameter.
#'
#' @return out A list with element a1 and b1, the shape parameters for the posterior Beta distribution.
#' @examples
#' BetaBernoulli(y = numeric(), a0 = 0.5, b0 = 0.5)
#' BetaBernoulli(y = c(1,1,1,0,0), a0 = 0.5, b0 = 0.5)

BetaBernoulli <- function(y = numeric(), a0 = 0.5, b0 = 0.5){

# maybe include a message if wrong input given

  n <- length(y)

  sumy <- sum(y)
  a1 <- a0 + sumy # update first hyperparameter (shape 1)
  b1 <- b0 + n - sumy # update second hyperparameter (shape 2)

  #
  out <- list(a1 = a1, b1 = b1)

  return(out)

}
