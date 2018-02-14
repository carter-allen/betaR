#' Plots probability mass function of beta binomial
#' @param n The number of trials
#' @param alpha Number of prior successes
#' @param beta Number of prior failures
#' @return The probability a beta binomial equals x
#' @examples
#' dbb_plot(10,5,5)
dbb_plot <- function(n,alpha,beta)
{
  xs = seq(0,n,by = 1) # support
  p.x = dbetbin(xs,n,alpha,beta) # p(x)
  bbs = as_tibble(cbind(xs,p.x)) # combine for ggplot

  ggplot(data = bbs, aes(x = xs, y = p.x)) +
    geom_col() +
    scale_x_discrete(limits = xs)
}
