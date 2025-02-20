% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtrackr.R
\name{p_transmute}
\alias{p_transmute}
\title{dplyr modifying operations}
\usage{
p_transmute(.data, ..., .messages = "", .headline = "", .tag = NULL)
}
\arguments{
\item{.data}{A data frame, data frame extension (e.g. a tibble), or a
lazy data frame (e.g. from dbplyr or dtplyr). See \emph{Methods}, below, for
more details.}

\item{...}{<\code{\link[rlang:args_data_masking]{data-masking}}> Name-value pairs.
The name gives the name of the column in the output.

The value can be:
\itemize{
\item A vector of length 1, which will be recycled to the correct length.
\item A vector the same length as the current group (or the whole data frame
if ungrouped).
\item \code{NULL}, to remove the column.
\item A data frame or tibble, to create multiple columns in the output.
}}

\item{.messages}{a set of glue specs. The glue code can use any global
variable, grouping variable, \{.new_cols\} or \{.dropped_cols\} for changes to
columns, \{.cols\} for the output column names, or \{.strata\}. Defaults to nothing.}

\item{.headline}{a headline glue spec. The glue code can use any global
variable, grouping variable, \{.new_cols\}, \{.dropped_cols\}, \{.cols\} or \{.strata\}.
Defaults to nothing.}

\item{.tag}{if you want the summary data from this step in the future then
give it a name with .tag.}
}
\value{
the \code{.data} dataframe after being modified by the \code{dplyr} equivalent
function, but with the history graph updated with a new stage if the
\code{.messages} or \code{.headline} parameter is not empty.
}
\description{
See \code{\link[dplyr:mutate]{dplyr::mutate()}}, \code{\link[dplyr:count]{dplyr::add_count()}}, \code{\link[dplyr:count]{dplyr::add_tally()}},
\code{\link[dplyr:transmute]{dplyr::transmute()}}, \code{\link[dplyr:select]{dplyr::select()}}, \code{\link[dplyr:relocate]{dplyr::relocate()}},
\code{\link[dplyr:rename]{dplyr::rename()}} \code{\link[dplyr:rename]{dplyr::rename_with()}}, \code{\link[dplyr:arrange]{dplyr::arrange()}} for more details
on underlying functions. \code{dtrackr} provides equivalent functions for
mutating, selecting and renaming a data set which act in the same way as
\code{dplyr}. \code{mutate} / \code{select} / \code{rename} generally don't add anything in terms
of provenance of data so the default behaviour is to miss these out of the
\code{dtrackr} history. This can be overridden with the \code{.messages}, or
\code{.headline} values in which case they behave just like a \code{comment()}.
}
\examples{
library(dplyr)
library(dtrackr)

# mutate and other functions are unitary operations that generally change
# the structure but not size of a dataframe. In dtrackr these are by ignored
# by default but we can change that so that their behaviour is obvious.

# In this example we compare the column names of the input and the
# output to identify the new columns created by the transmute operation as
# the `.new_cols` variable
# Here we do the same for a transmute()
iris \%>\%
  track() \%>\%
  group_by(Species, .add=TRUE) \%>\%
  transmute(
    sepal.w = Sepal.Width-1,
    sepal.l = Sepal.Length+1,
    .messages="{.new_cols}",
    .headline="New columns from transmute:") \%>\%
  history()

}
\seealso{
dplyr::transmute()
}
