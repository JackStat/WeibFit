#' Compute Fisher's Information Matrix
#' 
#' @param beta An estimate of beta.
#' @param x The data
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

Fisher <- function(beta, x){
  matrix(c(-df2.theta(beta, x), -df2.beta.theta(beta, x),
-df2.beta.theta(beta, x),-df2.beta(beta, x) ), nrow=2)
}
