\name{findCols}
\alias{findCols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{extract classes from classInterval object}
\description{
  This helper function is a wrapper for \code{findInterval} to extract classes from a "classInterval" object
}
\usage{
findCols(clI, factor = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clI}{a "classIntervals" object}
  \item{factor}{default "FALSE", if "TRUE" returns cols as a factor with intervals as labels}
}

\value{
  an integer vector of class indices
}
\author{Roger Bivand <Roger.Bivand@nhh.no>}

\seealso{\code{\link{classIntervals}}, \code{\link{findInterval}}}
\examples{
xvar <- c(22361, 9573, 4836, 5309, 10384, 4359, 11016, 4414, 3327, 3408, 
  17816, 6909, 6936, 7990, 3758, 3569, 21965, 3605, 2181, 1892, 
  2459, 2934, 6399, 8578, 8537, 4840, 12132, 3734, 4372, 9073, 
  7508, 5203)
findCols(classIntervals(xvar, 5, "sd"), factor = FALSE)
findCols(classIntervals(xvar, 5, "sd"), factor = TRUE)
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
print(findCols(fix5, factor = TRUE))
}
}
\keyword{spatial}
