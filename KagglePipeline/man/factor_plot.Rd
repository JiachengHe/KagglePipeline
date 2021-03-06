% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_plot.R
\name{factor_plot}
\alias{factor_plot}
\title{Plot the counts of each category or the average values of y for each category}
\usage{
factor_plot(df, facVar, yVar = NULL, type = "box", ylim = NULL)
}
\arguments{
\item{df}{The data frame which contains the variables}

\item{facVar}{The name of the factor variable}

\item{yVar}{The name of the left-hand-side target variable}

\item{type}{type="identity" to plot average y; type="count" to plot the frequency}

\item{ylim}{}
}
\description{
Provide guidence of how to process the factor variable in the next steps (e.g. We might
want to get rid of the categories with rare frequencey, or combine the categories with similar average y)
}
\author{
Jiacheng He
}
