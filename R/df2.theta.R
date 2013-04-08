#' Compute the Value for the Second Derivative of Theta
#' 
#' 
#' @param beta An estimate of beta.
#' @param x The data
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

df2.theta <- function(beta, x){
	n<-length(x)
	theta=(sum(x^beta/n)^(1/beta))
	(n*beta)/theta^2 - sum((x/theta)^beta*(beta/theta^2)*(beta+1))
}
