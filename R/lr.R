#' Compute the Likelihood-Ratio test for a Weibull Distribution
#' 
#' @param x The data.
#' @param null Value under the null hypothesis.
#' @param digits The number of decimal digits.
#' 
#' @details This function is a generalized likelihood ratio test that allows for differnt specifications of the null hypothesis.
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

lr <- function(null, x, digits=3){
  
  nn <- newton.raph(f=LL, df=df.beta, d2f=df2.beta, start=2, tol=.00001, x)
  alt <- nn[["Estimate"]]
  
  unres<-LL(alt, x)
  res<-LL(null, x)
  lambda<-LL(null, x)-LL(alt, x)
  lratio<--2*lambda
  p<-1-pchisq(lratio, 1)
  
  round(c("Under HO"=unres, "Under HA"=res, "Lambda"=lambda,"-2Log Lambda"=lratio, "p-value"=p), digits)
  
}
