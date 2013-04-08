#' Compute the Cross Partial of Weibull's Theta and Beta.
#' 
#' @param beta An estimate of beta
#' @param x The data
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

df2.beta.theta <- function(beta, x){
	n<-length(x)
	theta=(sum(x^beta/n)^(1/beta))
	-(n/theta) + sum( (x/theta)^beta*(1/theta)*( beta*log(x/theta)+1) )
}
