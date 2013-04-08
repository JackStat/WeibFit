#' Compute Confidence Intervals for Theta
#' 
#' @param beta An estimate of beta.
#' @param theta An estimate of theta.
#' @param conf Confidence Level.
#' @param x The data
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

theta.ci <- function(beta, theta, conf=.95, x){
	conf.int <- 1-(1-conf)/2
	se.t <- sqrt(solve(Fisher(beta, x))[1,1])
	lci.t <- theta/(exp(qnorm(conf.int)*se.t/theta))
	uci.t <- theta*exp(qnorm(conf.int)*se.t/theta)
	
	c(theta, se.t, lci.t, uci.t)
}
