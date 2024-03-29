% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BetaBernoulli.R
\name{BetaBernoulli}
\alias{BetaBernoulli}
\title{Calculates conjugate Beta posterior from Beta prior and Bernoulli data}
\usage{
BetaBernoulli(y = numeric(), a0 = 0.5, b0 = 0.5)
}
\arguments{
\item{y}{A vector of binary (0 or 1) data, can be empty.}

\item{a0}{A positive number, the Beta prior first shape parameter.}

\item{b0}{A postive number, the Beta prior second shape parameter.}
}
\value{
out A list with element a1 and b1, the shape parameters for the posterior Beta distribution.
}
\description{
Calculates conjugate Beta posterior from Beta prior and Bernoulli data
}
\examples{
BetaBernoulli(y = numeric(), a0 = 0.5, b0 = 0.5)
BetaBernoulli(y = c(1,1,1,0,0), a0 = 0.5, b0 = 0.5)
}
