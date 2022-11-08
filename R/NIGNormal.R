
#' Title
#'
#' @param y
#' @param mu0
#' @param B0
#' @param a0
#' @param d0
#'
#' @return
#' @export
#'
#' @examples
NIGNormal <- function(y = numeric(), mu0 = 0.0, B0=1, a0=3, d0=1){

  # maybe include a message if wrong input given

  n <- length(y)
  sumy <- sum(y)
  sumy2 <- sum(y^2)

  B1 <- 1/(n+1/B0)
  mu1 <- B1*(sumy + mu0/B0)

  a1 <- n + a0
  d1 <- d0 + sumy2 + mu0^2/B0 - mu1^2/B1

  out <- list(mu1 = mu1, B1 = B1, a1 = a1, d1 = d1)

  return(out)

}
