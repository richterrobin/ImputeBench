% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_1D_real_missingness.R
\name{plot_1D_real_missingness}
\alias{plot_1D_real_missingness}
\title{Plotting the Correlation between Observed Data and the Missingness Pattern (1D)}
\usage{
plot_1D_real_missingness(data, miss.clm = 1, data.clm = 2, title = NULL)
}
\arguments{
\item{data}{A matrix with numeric entries or \code{NA} entries marking missing entries (ground truth matrix).}

\item{miss.clm}{An integer, indicating the column whose missingness pattern is to be plotted. Default is \code{1}.}

\item{data.clm}{An integer, indicating the column whose data distribution is to be plotted. Default is \code{2}.}

\item{title}{A character string controlling the title of the plot. Default is \code{NULL}.}
}
\value{
A ggplot2 plot.
}
\description{
Plotting the distribution of observed entries of a given data matrix with respect to the missingness pattern of an
additional column of the same matrix, in 1 dimension using ggplot2.
}
\details{
Plotting the missingness pattern of \code{miss.clm} with respect to the data distribution of \code{data.clm}.
All rows with a missing value in \code{data.clm} are left out. In the case that \code{data.clm} is continuous, the output plot is a
boxplot comparing the subdistributions of rows with a missing entry in \code{miss.clm} and those with an existing entry in
\code{miss.clm}. In the case that \code{data.clm} is discrete, the output plot is a filled bar plot with the missingness in
\code{miss.clm} color coded (Additionally one finds the total number of data rows for each value-missingness-pair on the top
and bottom of each bar).
}
