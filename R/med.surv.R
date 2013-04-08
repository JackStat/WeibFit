#' Compute Median Survival
#' 
#' @param x The data.
#' @param conf Confidence level.
#' @param digits The number of decimal digits.
#' 
#' @return
#'  \item{Estimate}{The Estimate of median survival.}
#'  \item{SE}{The standard error for the median survival estimate.}
#'  \item{.95 LCI}{95\% lower confidence estimate.}
#'  \item{.95 UCI}{95\% upper confidence estimate.}
#' @details This formulation was derived by setting the cumulative density function of the Weibull distribution to \eqn{1/2} and then solving for the value \eqn{t}.  The delta method is employed to derive a standard error and confidence limits. 
#'  
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

med.surv <- function(x, conf=.95, digits=3){
	conf.int <- 1-(1-conf)/2
  
	n <- length(x)
	nn <- newton.raph(f=LL, df=df.beta, d2f=df2.beta, start=2, tol=.00001, x)
	beta <- nn[["Estimate"]]
	theta <- (sum(x^beta/n)^(1/beta))
		
	median <- theta*(log(2)^(1/beta))
	
	d.theta <- log(2)^(1/beta)
	d.beta <- -(theta*log(log(2))*log(2)^(1/beta))/beta^2
	
	var.med <- (d.beta^2)*solve(Fisher(beta, x))[2,2]+
		(d.theta^2)*solve(Fisher(beta, x))[1,1]+
		2*d.beta*d.theta*solve(Fisher(beta, x))[1,2]

	se.m <- sqrt(var.med)
	
	lci.m <- median-qnorm(conf.int)*se.m
	uci.m <- median+qnorm(conf.int)*se.m
	
	
	round(c("Estimate"=median, "SE"=se.m, ".95 LCI"=lci.m, ".95 UCI"=uci.m), digits)
}
