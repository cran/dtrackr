% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtrackr.R
\name{ungroup.trackr_df}
\alias{ungroup.trackr_df}
\title{Remove a stratification from a data set}
\usage{
\method{ungroup}{trackr_df}(
  x,
  ...,
  .messages = .defaultMessage(),
  .headline = .defaultHeadline(),
  .tag = NULL
)
}
\arguments{
\item{x}{A \code{\link[dplyr:tbl]{tbl()}}}

\item{...}{In \code{group_by()}, variables or computations to group by.
Computations are always done on the ungrouped data frame.
To perform computations on the grouped data, you need to use
a separate \code{mutate()} step before the \code{group_by()}.
Computations are not allowed in \code{nest_by()}.
In \code{ungroup()}, variables to remove from the grouping.}

\item{.messages}{a set of glue specs. The glue code can use any any global
variable, or \{.count\}. the default is "total \{.count\} items"}

\item{.headline}{a headline glue spec. The glue code can use \{.count\} and
\{.strata\}.}

\item{.tag}{if you want the summary data from this step in the future then
give it a name with .tag.}
}
\value{
the .data dataframe but ungrouped with the history graph
updated showing the ungroup operation as a new stage.
}
\description{
Un-grouping a data set logically combines the different arms. In the history
this joins any stratified branches and acts as a specific type of \code{\link[=status]{status()}},
allowing you to generate some summary statistics about the un-grouped data.
See \code{\link[dplyr:group_by]{dplyr::ungroup()}}.
}
\examples{
library(dplyr)
library(dtrackr)

tmp = iris \%>\% group_by(Species) \%>\% comment("A test")
tmp \%>\% ungroup(.messages="{.count} items in combined") \%>\% history()
}
\seealso{
dplyr::ungroup()
}
