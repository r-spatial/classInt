\name{getBclustClassIntervals}
\alias{getBclustClassIntervals}
\alias{getHclustClassIntervals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Change breaks in a "classIntervals" object}
\description{
 Because "classIntervals" objects of style "hclust" or "bclust" contain hierarchical classification trees in their "par" attribute, different numbers of classes can be chosen without repeating the initial classification. This function accesses the "par" attribute and modifies the "brks" member of the returned "classIntervals" object.
}
\usage{
getBclustClassIntervals(clI, k)
getHclustClassIntervals(clI, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clI}{a "classIntervals" object}
  \item{k}{number of classes required}
}

\value{
  a "classIntervals" object with a "modified" attribute set
}

\author{Roger Bivand <Roger.Bivand@nhh.no>}

\seealso{\code{\link{classIntervals}}}
\examples{
data(jenks71)
pal1 <- c("wheat1", "red3")
opar <- par(mfrow=c(2,2))
hCI5 <- classIntervals(jenks71$jenks71, n=5, style="hclust", method="complete")
plot(attr(hCI5, "par"))
plot(hCI5, pal=pal1, main="hclust k=5")
plot(getHclustClassIntervals(hCI5, k=7), pal=pal1, main="hclust k=7")
plot(getHclustClassIntervals(hCI5, k=9), pal=pal1, main="hclust k=9")
par(opar)
set.seed(1)
bCI5 <- classIntervals(jenks71$jenks71, n=5, style="bclust")
plot(attr(bCI5, "par"))
opar <- par(mfrow=c(2,2))
plot(getBclustClassIntervals(bCI5, k=3), pal=pal1, main="bclust k=3")
plot(bCI5, pal=pal1, main="bclust k=5")
plot(getBclustClassIntervals(bCI5, k=7), pal=pal1, main="bclust k=7")
plot(getBclustClassIntervals(bCI5, k=9), pal=pal1, main="bclust k=9")
par(opar)
}
\keyword{spatial}
