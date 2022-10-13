GammaPoisson.f <- function(y = numeric(), a0, b0,
                           pr = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975), plot = FALSE){

  # would be good to make input values more robust

  # if(is.na(y) == TRUE){
  #   y <- numeric()
  # }

  n <- length(y)

  sumy <- sum(y)
  a1 <- a0 + sumy # update first hyperparameter (shape)
  b1 <- b0 + n    # update second hyperparameter (rate)

  ############ we have found the posterior distribution above ############
  ############ now we want to output various properties of it ############

  # posterior (updated) Gamma distribution mean and variance
  m <- a1/b1 # mean
  v <- a1/(b1^2) # variance

  # posterior (updated) Gamma distribution quantiles
  nn <- length(pr)
  q <- rep(NA, nn)
  for(i in 1:nn){
    q[i] <- qgamma(pr[i], shape = a1, rate = b1)
  }

  out <- list(a1 = a1, b1 = b1, m = m, v = v, pr = pr, q = q, pdfplot = NA)

  if(plot == TRUE){
    # would be good to have some way to automate the y-limits on the plot
    # perhaps check if end points are higher than the middle and restrict to the


    xlims <- qgamma(c(0.001,0.999), shape = a1, rate = b1)
    xgrid <- seq(xlims[1], xlims[2], 0.001)

    gammapdf <- dgamma(xgrid, shape = a1, rate = b1)
    dt <- tibble(theta = xgrid, pdf = gammapdf)

    pdfplot <- dt %>% ggplot(aes(theta, pdf)) +
      geom_line(colour = "blue") +
      geom_hline(yintercept = 0)
    out$pdfplot <- pdfplot
  }

  return(out)

}
