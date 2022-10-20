#' Calculates conjugate Gamma posterior from Gamma prior and Exponential data
#'
#' @param y A vector of positive numeric data values, can be empty.
#' @param a0 A positive number, the Gamma prior shape parameter.
#' @param b0 A positive number, the Gamma prior rate parameter (=1/scale parameter).
#' @return out A list with element a1 and b1, the shape parameters for the posterior Gamma distribution.
#' @examples
#' GammaExponential(y = numeric(), a0 = 2, b0 = 1)
#' GammaExponential(y = c(1.215, 3.915, 2.519, 2.593, 6.924), a0 = 2, b0 = 1.0)

GammaExponential <- function(y = numeric(), a0 = 2.0, b0 = 1.0){

  # maybe include a message if wrong input given

  n <- length(y)

  sumy <- sum(y)
  a1 <- a0 + n # update first hyperparameter (shape)
  b1 <- b0 + sumy # update second hyperparameter (rate)

  #
  out <- list(a1 = a1, b1 = b1)

  return(out)

}
