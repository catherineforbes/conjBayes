#' Calculates conjugate Beta posterior from Beta prior and Binomial data
#'
#' @param y An integer data value, the number of successes in n iid Bernoulli trials. Can be empty.
#' @param a0 A positive number, the Beta prior first shape parameter.
#' @param b0 A postive number, the Beta prior second shape parameter.
#' @param n
#'
#' @return out A list with element a1 and b1, the shape parameters for the posterior Beta distribution.
#' @examples
#' BetaBernoulli(y = numeric(), n=100, a0 = 0.5, b0 = 0.5)
#' BetaBernoulli(y = 4, n=10  a0 = 0.5, b0 = 0.5)

BetaBinomial <- function(y = numeric(), n=10, a0 = 0.5, b0 = 0.5){

# maybe include a message if wrong input given

  a1 <- a0 + y # update first hyperparameter (shape 1)
  b1 <- b0 + n - y # update second hyperparameter (shape 2)

  #
  out <- list(a1 = a1, b1 = b1)

  return(out)

}
