#' Probability mass function of a beta binomial
#'
#' Calculates probability mass function of beta binomial
#' @param x An integer less than or equal to n
#' @param n The number of trials
#' @param alpha Number of prior successes
#' @param beta Number of prior failures
#' @return The probability a beta binomial equals x
#' @examples
#' dbetbin(10,20,10,10)
dbetbin <- function(x,n,alpha,beta)
{
  # Check for valid parameters
  if(n < 0){
    stop("n cannot be less than 0")
  }

  if(!all(x <= n)){
    stop("x cannot be greater than n")
  }

  if(alpha < 0){
    stop("alpha cannot be less than 0")
  }

  if(beta < 0){
    stop("beta cannot be less than 0")
  }

  # Calculate p(x) according to pmf
  p.x = choose(n,x)*beta(x+alpha,beta+n-x)*(1/beta(alpha,beta))
  return(p.x)
}
