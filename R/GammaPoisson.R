#' Calculates conjugate Gamma posterior from Gamma prior and Poisson data
#'
#' @param y A vector of non-negative integer count data, can be empty.
#' @param a0 A positive number, the Gamma prior shape parameter.
#' @param b0 A positive number, the Gamma prior rate parameter (=1/scale parameter).
#' @return out A list with element a1 and b1, the shape parameters for the posterior Gamma distribution.
#' @examples
#' GammaPoisson(y = numeric(), a0 = 2, b0 = 1)
#' GammaPoisson(y = c(1, 3, 5, 6, 5), a0 = 2, b0 = 1.0)

GammaPoisson <- function(y = numeric(), a0 = 2.0, b0 = 1.0){

  # maybe include a message if wrong input given

  n <- length(y)

  sumy <- sum(y)
  a1 <- a0 + sumy # update first hyperparameter (shape)
  b1 <- b0 + n # update second hyperparameter (rate)

  #
  out <- list(a1 = a1, b1 = b1)

  return(out)

}
