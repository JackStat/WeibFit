\name{weib.fit}
\alias{weib.fit}
\title{Estimate Parameters of a Weibull Distribution}
\usage{
  weib.fit(x, digits = 3)
}
\arguments{
  \item{x}{The data.}

  \item{digits}{The number of decimal digits.}
}
\description{
  Estimate Parameters of a Weibull Distribution
}
\details{
  Fits the Weibull distribution using the Newton-Raphson
  method.  The first and second derivatives with respect to
  beta of the log likelihood of a weibull distribution are
  used to estimate the parameters.
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}

