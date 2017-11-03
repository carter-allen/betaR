#' Plots probability mass function of beta binomial
#' @param n The number of trials
#' @param p The probability of success on each trial
#' @return The probability a binomial equals x
#' @examples
#' dbinom_plot(10,0.5)
dbinom_plot <- function(n,p)
{
  xs = seq(0,n,by = 1) # support
  p.x.binom = dbinom(xs,n,p) # p.x
  binoms = as_tibble(cbind(xs,p.x.binom)) # combine for ggplot

  ggplot(data = binoms,aes(x = xs,y = p.x.binom)) +
    geom_col() +
    scale_x_discrete(limits = xs)
}
