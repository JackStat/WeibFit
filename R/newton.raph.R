#' Newton Raphson Alogorithm
#' 
#' @param f The function that you want to optimize.
#' @param df the 1st derivative of the function.
#' @param d2f the 2nd derivative of the function.
#' @param start The starting value for optimization.
#' @param tol The level of tolerance desired for convergence.
#' @param x The data
#' 
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export


newton.raph <- function(f, df, d2f, start, tol=0.000001, x){
  
  new <- start + 10*tol
  iter <- 0
  while(abs(start - new) > tol){
    iter <- iter+1
    new <- start
    start <- start - df(start, x)/d2f(start, x)
  }
  result<-c(Estimate=start, Iterations=iter, Likelihood=f(start, x))
  return(result)
}
