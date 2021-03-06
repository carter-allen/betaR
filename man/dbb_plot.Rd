% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbb_plot.R
\name{dbb_plot}
\alias{dbb_plot}
\title{Plots probability mass function of beta binomial}
\usage{
dbb_plot(n, alpha, beta)
}
\arguments{
\item{n}{The number of trials}

\item{alpha}{Number of prior successes}

\item{beta}{Number of prior failures}
}
\value{
The probability a beta binomial equals x
}
\description{
Plots probability mass function of beta binomial
}
\examples{
dbb_plot(10,5,5)
}
