#' Cumulative distribution function of a beta binomial
#'
#' Calculates cumulative distribution function of beta binomial
#' @param x An integer less than or equal to n
#' @param n The number of trials
#' @param alpha Number of prior successes
#' @param beta Number of prior failures
#' @return The probability a beta binomial is less than or equal to x
#' @examples
#' pbetbin(10,20,10,10)
pbetbin <- function(x,n,alpha,beta)
{
  # dbinbet already checks for valid parameters
  # define support [0,x]
  xs = seq(0,x,by = 1)
  return(sum(dbetbinom(xs,n,alpha,beta)))
}
