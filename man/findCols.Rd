\name{findCols}
\alias{findCols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{extract classes from classInterval object}
\description{
  This helper function is a wrapper for \code{findInterval} to extract classes from a "classInterval" object
}
\usage{
findCols(clI)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clI}{a "classIntervals" object}
}

\value{
  an integer vector of class indices
}
\author{Roger Bivand <Roger.Bivand@nhh.no>}

\seealso{\code{\link{classIntervals}}, \code{\link{findInterval}}}
\examples{
if (!require("spData", quietly=TRUE)) {
  message("spData package needed for examples")
  run <- FALSE
} else {
  run <- TRUE
}
if (run) {
data(jenks71, package="spData")
fix5 <- classIntervals(jenks71$jenks71, n=5, style="fixed",
 fixedBreaks=c(15.57, 25, 50, 75, 100, 155.30))
print(fix5)
}
if (run) {
print(findCols(fix5))
}
}
\keyword{spatial}
