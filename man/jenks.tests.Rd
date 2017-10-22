\name{jenks.tests}
\alias{jenks.tests}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Indices for assessing class intervals}
\description{
  The function returns values of two indices for assessing class intervals: the goodness of variance fit measure, and the tabular accuracy index; optionally the overview accuracy index is also returned if the \code{area} argument is not missing.
}
\usage{
jenks.tests(clI, area)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clI}{a "classIntervals" object}
  \item{area}{an optional vector of object areas if the overview accuracy index is also required}
}
\details{
  The goodness of variance fit measure is given by Armstrong et al. (2003, p. 600) as:

\deqn{GVF = 1 - \frac{\sum_{j=1}^{k}\sum_{i=1}^{N_j}{(z_{ij} - \bar{z}_j)}^2}{\sum_{i=1}^{N}{(z_{i} - \bar{z})}^2}}

where the \eqn{z_{i}, i=1,\ldots,N} are the observed values, \eqn{k} is the number of classes, \eqn{\bar{z}_j} the class mean for class \eqn{j}, and \eqn{N_j} the number of counties in class \eqn{j}.

  The tabular accuracy index is given by Armstrong et al. (2003, p. 600) as:

\deqn{TAI = 1 - \frac{\sum_{j=1}^{k}\sum_{i=1}^{N_j}{|z_{ij} - \bar{z}_j|}}{\sum_{i=1}^{N}{|z_{i} - \bar{z}|}}}

  The overview accuracy index for polygon observations with known areas is given by Armstrong et al. (2003, p. 600) as:

\deqn{OAI = 1 - \frac{\sum_{j=1}^{k}\sum_{i=1}^{N_j}{|z_{ij} - \bar{z}_j| a_{ij}}}{\sum_{i=1}^{N}{|z_{i} - \bar{z}| a_i}}}

where \eqn{a_i, i=1,\ldots,N} are the polygon areas, and as above the \eqn{a_{ij}} term is indexed over \eqn{j=1,\ldots,k} classes, and \eqn{i=1,\ldots,N_j} polygons in class \eqn{j}.
}
\value{
  a named vector of index values
}
\references{Armstrong, M. P., Xiao, N., Bennett, D. A., 2003. "Using genetic algorithms to create multicriteria class intervals for choropleth maps". Annals, Association of American Geographers, 93 (3), 595--623; Jenks, G. F., Caspall, F. C., 1971. "Error on choroplethic maps: definition, measurement, reduction". Annals, Association of American Geographers, 61 (2), 217--244}
\author{Roger Bivand <Roger.Bivand@nhh.no>}

\seealso{\code{\link{classIntervals}}}
\examples{
data(jenks71)
fix5 <- classIntervals(jenks71$jenks71, n=5, style="fixed",
 fixedBreaks=c(15.57, 25, 50, 75, 100, 155.30))
jenks.tests(fix5, jenks71$area)
q5 <- classIntervals(jenks71$jenks71, n=5, style="quantile")
jenks.tests(q5, jenks71$area)
set.seed(1)
k5 <- classIntervals(jenks71$jenks71, n=5, style="kmeans")
jenks.tests(k5, jenks71$area)
h5 <- classIntervals(jenks71$jenks71, n=5, style="hclust", method="complete")
jenks.tests(h5, jenks71$area)
jenks.tests(getHclustClassIntervals(h5, k=7), jenks71$area)
jenks.tests(getHclustClassIntervals(h5, k=9), jenks71$area)
set.seed(1)
b5 <- classIntervals(jenks71$jenks71, n=5, style="bclust")
jenks.tests(b5, jenks71$area)
jenks.tests(getBclustClassIntervals(b5, k=7), jenks71$area)
jenks.tests(getBclustClassIntervals(b5, k=9), jenks71$area)
}
\keyword{spatial}
