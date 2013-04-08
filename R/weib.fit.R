#' Estimate Parameters of a Weibull Distribution
#' 
#' @param x The data.
#' @param digits The number of decimal digits.
#' 
#' @details Fits the Weibull distribution using the Newton-Raphson method.  The first and second derivatives with respect to beta of the log likelihood of a weibull distribution are used to estimate the parameters.
#' 
#'
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

weib.fit <- function(x, digits=3){
  n <- length(x)
  nn <- newton.raph(f=LL, df=df.beta, d2f=df2.beta, start=2, tol=.00001, x)
  beta <- nn[["Estimate"]]
  theta <- (sum(x^beta/n)^(1/beta))
  
  Theta <- theta.ci(beta, theta, .95, x)
  Beta <- beta.ci(beta, theta, .95, x)
  
  pretab <- rbind(Theta, Beta)
  colnames(pretab) <- c("Estimate", "SE", ".95 LCI", ".95 UCI")
  
  round(pretab, digits)
}
