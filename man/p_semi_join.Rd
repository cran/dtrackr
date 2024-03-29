% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtrackr.R
\name{p_semi_join}
\alias{p_semi_join}
\title{Semi join}
\usage{
p_semi_join(
  x,
  y,
  ...,
  .messages = c("{.count.lhs} on LHS", "{.count.rhs} on RHS",
    "{.count.out} in intersection"),
  .headline = "Semi join by {.keys}"
)
}
\arguments{
\item{x, y}{A pair of data frames, data frame extensions (e.g. a tibble), or
lazy data frames (e.g. from dbplyr or dtplyr). See \emph{Methods}, below, for
more details.}

\item{...}{
  Arguments passed on to \code{\link[dplyr:filter-joins]{dplyr::semi_join}}
  \describe{
    \item{\code{x,y}}{A pair of data frames, data frame extensions (e.g. a tibble), or
lazy data frames (e.g. from dbplyr or dtplyr). See \emph{Methods}, below, for
more details.}
    \item{\code{by}}{A join specification created with \code{\link[dplyr:join_by]{join_by()}}, or a character
vector of variables to join by.

If \code{NULL}, the default, \verb{*_join()} will perform a natural join, using all
variables in common across \code{x} and \code{y}. A message lists the variables so
that you can check they're correct; suppress the message by supplying \code{by}
explicitly.

To join on different variables between \code{x} and \code{y}, use a \code{\link[dplyr:join_by]{join_by()}}
specification. For example, \code{join_by(a == b)} will match \code{x$a} to \code{y$b}.

To join by multiple variables, use a \code{\link[dplyr:join_by]{join_by()}} specification with
multiple expressions. For example, \code{join_by(a == b, c == d)} will match
\code{x$a} to \code{y$b} and \code{x$c} to \code{y$d}. If the column names are the same between
\code{x} and \code{y}, you can shorten this by listing only the variable names, like
\code{join_by(a, c)}.

\code{\link[dplyr:join_by]{join_by()}} can also be used to perform inequality, rolling, and overlap
joins. See the documentation at \link[dplyr:join_by]{?join_by} for details on
these types of joins.

For simple equality joins, you can alternatively specify a character vector
of variable names to join by. For example, \code{by = c("a", "b")} joins \code{x$a}
to \code{y$a} and \code{x$b} to \code{y$b}. If variable names differ between \code{x} and \code{y},
use a named character vector like \code{by = c("x_a" = "y_a", "x_b" = "y_b")}.

To perform a cross-join, generating all combinations of \code{x} and \code{y}, see
\code{\link[dplyr:cross_join]{cross_join()}}.}
    \item{\code{copy}}{If \code{x} and \code{y} are not from the same data source,
and \code{copy} is \code{TRUE}, then \code{y} will be copied into the
same src as \code{x}.  This allows you to join tables across srcs, but
it is a potentially expensive operation so you must opt into it.}
  }}

\item{.messages}{a set of glue specs. The glue code can use any global
variable, \{.keys\} for the joining columns, \{.count.lhs\},
\{.count.rhs\}, \{.count.out\} for the input and output dataframes sizes
respectively}

\item{.headline}{a glue spec. The glue code can use any global variable,
\{.keys\} for the joining columns, \{.count.lhs\}, \{.count.rhs\},
\{.count.out\} for the input and output dataframes sizes respectively}
}
\value{
the join of the two dataframes with the history graph updated.
}
\description{
Mutating joins behave as \code{dplyr} joins, except the history graph of the two
sides of the joins is merged resulting in a tracked dataframe with the
history of both input dataframes. See \code{\link[dplyr:filter-joins]{dplyr::semi_join()}} for more details
on the underlying functions.
}
\examples{
library(dplyr)
library(dtrackr)
# Joins across data sets

# example data uses the dplyr starways data
people = starwars \%>\% select(-films, -vehicles, -starships)
films = starwars \%>\% select(name,films) \%>\% tidyr::unnest(cols = c(films))

lhs = people \%>\% track() \%>\% comment("People df {.total}")
rhs = films \%>\% track() \%>\% comment("Films df {.total}") \%>\%
  comment("a test comment")

# Semi join
join = lhs \%>\% semi_join(rhs, by="name") \%>\% comment("joined {.total}")
# See what the history of the graph is:
join \%>\% history() \%>\% print()
nrow(join)
# Display the tracked graph (not run in examples)
# join \%>\% flowchart()

}
\seealso{
dplyr::semi_join()
}
