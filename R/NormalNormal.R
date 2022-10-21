#' Calculates conjugate Normal posterior from Normal prior and Normal data
#'
#' @param y A vector of numeric data, can be empty.
#' @param mu0 A real number, the Normal prior mean parameter.
#' @param tau0 A positive number, the Normal prior standard (std) deviation parameter.
#' @param sigma A positive number, std deviation of the Normal data distribution, assumed known.
#'
#' @return out A list with element mu1 and tau1, the mean and std deviation parameters for the posterior Normal distribution.
#' @examples
#' NormalNormal(y = numeric(), mu0 = 2, tau0 = 1, sigma = 2)
#' NormalNormal(y = c(-1.807, 1.043, -1.700, 2.446, 2.367),  mu0 = 1, tau0 = 1, sigma = 2)

NormalNormal <- function(y = numeric(), mu0 = 0.0, tau0 = 1.0, sigma = 1.0){

  # maybe include a message if wrong input given

  n <- length(y)

  sumy <- sum(y)
  denom <- sigma^2 + n*tau0^2

  mu1 <- (mu0*sigma^2 + sumy*tau0^2)/denom # update first hyperparameter (mean)
  tau1 <- (sigma^2)*(tau0^2)/denom # update second hyperparameter (std deviation)

  #
  out <- list(mu1 = mu1, tau1 = tau1)

  return(out)

}
