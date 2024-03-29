% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtrackr.R
\name{p_get_as_dot}
\alias{p_get_as_dot}
\title{DOT output}
\usage{
p_get_as_dot(.data, fill = "lightgrey", fontsize = "8", colour = "black", ...)
}
\arguments{
\item{.data}{the tracked dataframe}

\item{fill}{the default node fill colour}

\item{fontsize}{the default font size}

\item{colour}{the default font colour}

\item{...}{not used}
}
\value{
a representation of the history graph in \code{Graphviz} dot format.
}
\description{
(advance usage) outputs a \code{dtrackr} history graph as a DOT string for rendering with \code{Graphviz}
}
\examples{
library(dplyr)
library(dtrackr)

tmp = iris \%>\% track() \%>\% comment(.tag = "step1") \%>\% filter(Species!="versicolor")
dot = tmp \%>\% group_by(Species) \%>\% comment(.tag="step2") \%>\% p_get_as_dot()
cat(dot)
}
