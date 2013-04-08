#' Compute Confidence Intervals for Beta
#' 
#' @param beta An estimate of beta.
#' @param theta An estimate of theta.
#' @param conf Confidence Level.
#' @param x The data
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export


beta.ci <- function(beta, theta, conf=.95, x){
	conf.int <- 1-(1-conf)/2
	se.b <- sqrt(solve(Fisher(beta, x))[2,2])
	lci.b <- beta-qnorm(conf.int)*se.b
	uci.b <- beta+qnorm(conf.int)*se.b
	
	c(beta, se.b, lci.b, uci.b)
}
