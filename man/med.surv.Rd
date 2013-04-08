\name{med.surv}
\alias{med.surv}
\title{Compute Median Survival}
\usage{
  med.surv(x, conf = 0.95, digits = 3)
}
\arguments{
  \item{x}{The data.}

  \item{conf}{Confidence level.}

  \item{digits}{The number of decimal digits.}
}
\value{
  \item{Estimate}{The Estimate of median survival.}
  \item{SE}{The standard error for the median survival
  estimate.} \item{.95 LCI}{95\% lower confidence
  estimate.} \item{.95 UCI}{95\% upper confidence
  estimate.}
}
\description{
  Compute Median Survival
}
\details{
  This formulation was derived by setting the cumulative
  density function of the Weibull distribution to \eqn{1/2}
  and then solving for the value \eqn{t}.  The delta method
  is employed to derive a standard error and confidence
  limits.
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}

