#' Compute the Slope of Beta at a Point
#' 
#' @param beta An estimate of Beta
#' @param x The data
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

df.beta <- function(beta, x){
	n<-length(x)
	theta<-(sum(x^beta/n)^(1/beta))
	n/beta + sum( log(x/theta) )-sum( (x/theta)^beta * log(x/theta) )
	}
