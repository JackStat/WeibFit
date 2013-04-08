#' Compute the Log Likelihood of the Weibull Distribution
#' 
#' @param beta An estimate of beta.
#' @param x The data
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

LL <- function(beta, x){
	n<-length(x)
	theta<-(sum(x^beta/n)^(1/beta))
	n * log(beta/theta) + (beta-1) * sum( log(x/theta) ) - sum( (x/theta)^beta )
}
