#' Plots probability density function of beta
#' @param alpha Number of prior successes
#' @param beta Number of prior failures
#' @param by The increment length to approximate the continuous support
#' @return The probability a beta equals x
#' @examples
#' dbeta_plot(10,5,5)
dbeta_plot <- function(alpha,beta,by=0.0025)
{
  thetas = seq(0,1,by = by) # support
  p.theta = dbeta(thetas,alpha,beta) # p.theta
  bets = as_tibble(cbind(thetas,p.theta)) # combine for ggplot

  ggplot(data = bets, aes(x = thetas,y = p.theta)) +
    geom_point()
}
