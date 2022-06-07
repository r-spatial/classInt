\name{Classify Intervals}
\alias{classify_intervals}
\title{Classify univariate vector to interval}
\usage{
classify_intervals(var, n, style = "quantile", rtimes = 3, ...,
 intervalClosure = c("left", "right"), dataPrecision = NULL,
 warnSmallN = TRUE, warnLargeN = TRUE, largeN = 3000L, samp_prop = 0.1,
 gr = c("[", "]"), factor = TRUE)

}

\arguments{
  \item{var}{a continuous numerical variable}
  \item{n}{number of classes required, if missing, \code{nclass.Sturges} is used; see also the "dpih" and "headtails" styles for automatic choice of the number of classes}
  \item{style}{chosen style: one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", or "maximum"}
  \item{rtimes}{number of replications of var to catenate and jitter; may be used with styles "kmeans" or "bclust" in case they have difficulties reaching a classification}
  \item{intervalClosure}{default \dQuote{left},  allows specification of whether partition intervals are closed on the left or the right (added by Richard Dunlap). Note that the sense of interval closure is hard-coded as \dQuote{right}-closed when\code{style="jenks"} (see Details below).}
  \item{dataPrecision}{default NULL, permits rounding of the interval endpoints (added by Richard Dunlap). The data precision used for printing interval values in the legend returned by \code{findColours}, and in the \code{print} method for classIntervals objects. If intervalClosure is \dQuote{left}, the value returned is \code{ceiling} of the data value multiplied by 10 to the dataPrecision power, divided by 10 to the dataPrecision power. The argument does not round \code{var}, the input variable.}
  \item{warnSmallN}{default TRUE, if FALSE, quietens warning for n >= nobs}
  \item{warnLargeN}{default TRUE, if FALSE large data handling not used}
  \item{largeN}{default 3000L, the QGIS sampling threshold; over 3000, the observations presented to "fisher" and "jenks" are either a \code{samp_prop=} sample or a sample of 3000, whichever is larger}
  \item{samp_prop}{default 0.1, QGIS 10\% sampling proportion}
  \item{gr}{default \code{c("[", "]")}, if the \pkg{units} package is available, \code{units::units_options("group")} may be used directly to give the enclosing bracket style}
  \item{\dots}{arguments to be passed to the functions called in each style}
  \item{factor}{default "TRUE", if "TRUE" returns cols as a factor with intervals as labels rather than integers}
}
\value{
A vector of same length as \code{var}. When \code{factor = FALSE} returns a factor where the levels are the interval of the observation. 
}
\description{
Given a numeric vector classify into numeric intervals. \code{classify_intervals()} is a wrapper of both \code{classIntervals()} and \code{findCols()}.
}

\seealso{
\code{\link{classIntervals}()},
\code{\link{findCols}()}
}

\examples{
xvar <- c(22361, 9573, 4836, 5309, 10384, 4359, 11016, 4414, 3327, 3408, 
  17816, 6909, 6936, 7990, 3758, 3569, 21965, 3605, 2181, 1892, 
  2459, 2934, 6399, 8578, 8537, 4840, 12132, 3734, 4372, 9073, 
  7508, 5203)
classIntervals(xvar, 5, "sd")
classify_intervals(xvar, 5, "sd", factor = FALSE)
classify_intervals(xvar, 5, "sd", factor = TRUE)

if (!require("spData", quietly=TRUE)) {
  message("spData package needed for examples")
  run <- FALSE
} else {
  run <- TRUE
}

if (run) {
  data("jenks71", package = "spData")
  x <- jenks71$jenks71
  classify_intervals(x, n = 5, style = "fisher")
}

}
