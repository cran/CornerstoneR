% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reshapeLong.R
\name{reshapeLong}
\alias{reshapeLong}
\title{Reshape Grouped Data to Long}
\usage{
reshapeLong(
  dataset = cs.in.dataset(),
  preds = cs.in.predictors(),
  resps = cs.in.responses(),
  scriptvars = cs.in.scriptvars(),
  return.results = FALSE,
  ...
)
}
\arguments{
\item{dataset}{[\code{\link{data.frame}}]\cr
Dataset with named columns. The names correspond to predictors and responses.}

\item{preds}{[\code{character}]\cr
Character vector of predictor variables.}

\item{resps}{[\code{character}]\cr
Character vector of response variables.}

\item{scriptvars}{[\code{list}]\cr
Named list of script variables set via the Cornerstone "Script Variables" menu.
For details see below.}

\item{return.results}{[\code{logical(1)}]\cr
If \code{FALSE} the function returns \code{TRUE} invisibly.
If \code{TRUE}, it returns a \code{\link{list}} of results.
Default is \code{FALSE}.}

\item{...}{[ANY]\cr
Additional arguments to be passed to 
\code{\link[data.table:melt.data.table]{melt}}
. Please consider possible script variables (\code{scriptvars}) to prevent duplicates.}
}
\value{
Logical [\code{TRUE}] invisibly and outputs to Cornerstone or, 
  if \code{return.results = TRUE}, \code{\link{list}} of 
  resulting \code{\link{data.frame}} object:
  \item{reshapeLong}{Dataset with reshaped data.}
}
\description{
Reshaping grouped data via \code{\link[data.table:melt.data.table]{melt}} to 'long' format. The
  responses are merged in one column, with its column name in an additional column.
  This column is split into multiple columns, if a split character is given.
  All predictors are merged multiple times corresponding to the number or responses.
}
\details{
One script variables is summarized in \code{scriptvars} list:\cr
  \describe{
    \item{split}{[\code{character(1)}]\cr
      Split character to split response names into multiple columns. Default is \dQuote{_}.}
  }
}
\examples{
# Data to transform:
library(data.table)
dtTest = data.table(i_1 = c(1:4, NA, 5), i_2 = c(51, 61, NA , 71, 81, 91)
                    , f1 = factor(sample(c(letters[1:3], NA), 6, TRUE))
                    , f2 = factor(c("z", "a", "x", "c", "x", "x"), ordered = TRUE)
                    )
# Reshape to long format:
reshapeLong(dtTest, c("i_1", "i_2"), c("f1", "f2"), list(split = "_"), return.results = TRUE)
}
