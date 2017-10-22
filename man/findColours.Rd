\name{findColours}
\alias{findColours}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{assign colours to classes from classInterval object}
\description{
  This helper function is a wrapper for \code{findCols} to extract classes from a "classInterval" object and assign colours from a palette created by \code{colorRampPalette} from the two or more colours given in the \code{pal} argument. It also returns two attributes for use in constructing a legend.
}
\usage{
findColours(clI, pal, under="under", over="over", between="-", 
 digits = getOption("digits"), cutlabels=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clI}{a "classIntervals" object}
  \item{pal}{a character vector of at least two colour names; \code{colorRampPalette} is used internally to create the required number of colours}
  \item{under}{character string value for "under" in legend if cutlabels=FALSE}
  \item{over}{character string value for "over" in legend if cutlabels=FALSE}
  \item{between}{character string value for "between" in legend if cutlabels=FALSE}
  \item{digits}{minimal number of significant digits in legend}
  \item{cutlabels}{use cut-style labels in legend}
}

\value{
 a character vector of colours with attributes: "table", a named frequency table; "palette", a character vector of colours corresponding to the specified breaks.
}
\author{Roger Bivand <Roger.Bivand@nhh.no}

\seealso{\code{\link{classIntervals}}, \code{\link{findInterval}}, \code{\link{findCols}}, \code{\link[grDevices]{colorRamp}}}
\examples{
data(jenks71)
mypal <- c("wheat1", "red3")
h5 <- classIntervals(jenks71$jenks71, n=5, style="hclust", method="complete")
findColours(h5, mypal)
findColours(getHclustClassIntervals(h5, k=7), mypal)
h5Colours <- findColours(h5, mypal)
plot(h5, mypal, main="Complete hierarchical clustering")
legend(c(95, 155), c(0.12, 0.4), fill=attr(h5Colours, "palette"),
 legend=names(attr(h5Colours, "table")), bg="white")
h5tab <- attr(h5Colours, "table")
legtext <- paste(names(h5tab), " (", h5tab, ")", sep="")
plot(h5, mypal, main="Complete hierarchical clustering (with counts)")
legend(c(95, 165), c(0.12, 0.4), fill=attr(h5Colours, "palette"),
 legend=legtext, bg="white")
}
\keyword{spatial}

