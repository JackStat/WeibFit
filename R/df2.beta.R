#' Compute the Value of the 2nd Derivative of Beta
#' 
#' @param beta An estimate of beta
#' @param x The data
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

df2.beta <- function(beta, x){
	n<-length(x)
	theta=(sum(x^beta/n)^(1/beta))
	-(n/beta^2) - sum( (x/theta)^beta * log(x/theta)^2 )
}
