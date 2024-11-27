\name{classIntervals}
\alias{classIntervals}
\alias{print.classIntervals}
\alias{plot.classIntervals}
\alias{nPartitions}
\alias{classIntervals2shingle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Choose univariate class intervals}
\description{
  The function provides a uniform interface to finding class intervals for continuous numerical variables, for example for choosing colours or symbols for plotting. Class intervals are non-overlapping, and the classes are left-closed --- see \code{findInterval}. Argument values to the style chosen are passed through the dot arguments. \code{classIntervals2shingle} converts a \code{classIntervals} object into a shingle. Labels generated in methods are like those found in \code{\link{cut}} unless cutlabels=FALSE.
}
\usage{
classIntervals(var, n, style = "quantile", rtimes = 3, ...,
 intervalClosure = c("left", "right"), dataPrecision = NULL,
 warnSmallN = TRUE, warnLargeN = TRUE, largeN = 3000L, samp_prop = 0.1,
 gr = c("[", "]"))
\method{plot}{classIntervals}(x, pal, ...)
\method{print}{classIntervals}(x, digits = getOption("digits"), ...,
 under="under", over="over", between="-", cutlabels=TRUE, unique=FALSE,big.mark=NULL)
nPartitions(x)
classIntervals2shingle(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{a continuous numerical variable}
  \item{n}{number of classes required, if missing, \code{nclass.Sturges} is used; see also the "dpih" and "headtails" styles for automatic choice of the number of classes}
  \item{style}{chosen style: one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", or "box"}
  \item{rtimes}{number of replications of var to catenate and jitter; may be used with styles "kmeans" or "bclust" in case they have difficulties reaching a classification}
  \item{intervalClosure}{default \dQuote{left},  allows specification of whether partition intervals are closed on the left or the right (added by Richard Dunlap). Note that the sense of interval closure is hard-coded as \dQuote{right}-closed when\code{style="jenks"} (see Details below).}
  \item{dataPrecision}{default NULL, permits rounding of the interval endpoints (added by Richard Dunlap). The data precision used for printing interval values in the legend returned by \code{findColours}, and in the \code{print} method for classIntervals objects. If intervalClosure is \dQuote{left}, the value returned is \code{ceiling} of the data value multiplied by 10 to the dataPrecision power, divided by 10 to the dataPrecision power. The argument does not round \code{var}, the input variable.}
  \item{warnSmallN}{default TRUE, if FALSE, quietens warning for n >= nobs}
  \item{warnLargeN}{default TRUE, if FALSE large data handling not used}
  \item{largeN}{default 3000L, the QGIS sampling threshold; if \code{warnLargeN}is TRUE and the number of observations is greater than \code{largeN}, the observations presented to "fisher" and "jenks" are either a \code{samp_prop=} sample of the observations, or a sample of \code{largeN}, whichever is smaller}
  \item{samp_prop}{default 0.1, QGIS 10\% sampling proportion}
  \item{gr}{default \code{c("[", "]")}, if the \pkg{units} package is available, \code{units::units_options("group")} may be used directly to give the enclosing bracket style}
  \item{\dots}{arguments to be passed to the functions called in each style}
  \item{x}{"classIntervals" object for printing, conversion to shingle, or plotting}
  \item{under}{character string value for "under" in printed table labels if cutlabels=FALSE}
  \item{over}{character string value for "over" in printed table labels if cutlabels=FALSE}
  \item{between}{character string value for "between" in printed table labels if cutlabels=FALSE}
  \item{digits}{minimal number of significant digits in printed table labels}
  \item{cutlabels}{default TRUE, use cut-style labels in printed table labels}
  \item{unique}{default FALSE; if TRUE, collapse labels of single-value classes}
  \item{big.mark}{default NULL; an object of class \code{character} to specify to 'thousands' separator}
  \item{pal}{a character vector of at least two colour names for colour coding the class intervals in an ECDF plot; \code{colorRampPalette} is used internally to create the correct number of colours}
}
\details{
  The "fixed" style permits a "classIntervals" object to be specified with given breaks, set in the \code{fixedBreaks} argument; the length of \code{fixedBreaks} should be n+1; this style can be used to insert rounded break values.

  The "sd" style chooses breaks based on \code{pretty} of the centred and scaled variables, and may have a number of classes different from n; the returned \code{par=} includes the centre and scale values. If argument \code{sd_m} is given as a numeric vector of multiples to apply to the standard deviation such as \code{c(-Inf, -2, -1, 0, 1, 2, +Inf)}

  The "equal" style divides the range of the variable into n parts.

  The "pretty" style chooses a number of breaks not necessarily equal to n using \code{pretty}, but likely to be legible; arguments to \code{pretty} may be passed through \code{\dots}.

  The "quantile" style provides quantile breaks; arguments to \code{quantile} may be passed through \code{\dots}.

  The "kmeans" style uses \code{kmeans} to generate the breaks; it may be anchored using \code{set.seed}; the \code{pars} attribute returns the kmeans object generated; if \code{kmeans} fails, a jittered input vector containing \code{rtimes} replications of \code{var} is tried --- with few unique values in \code{var}, this can prove necessary; arguments to \code{kmeans} may be passed through \code{\dots}.

  The "hclust" style uses \code{hclust} to generate the breaks using hierarchical clustering; the \code{pars} attribute returns the hclust object generated, and can be used to find other breaks using \code{getHclustClassIntervals}; arguments to \code{hclust} may be passed through \code{\dots}.

  The "bclust" style uses \code{bclust} to generate the breaks using bagged clustering; it may be anchored using \code{set.seed}; the \code{pars} attribute returns the bclust object generated, and can be used to find other breaks using \code{getBclustClassIntervals}; if \code{bclust} fails, a jittered input vector containing \code{rtimes} replications of \code{var} is tried --- with few unique values in \code{var}, this can prove necessary; arguments to \code{bclust} may be passed through \code{\dots}.

  The "fisher" style uses the algorithm proposed by W. D. Fisher (1958) and discussed by Slocum et al. (2005) as the Fisher-Jenks algorithm; added here thanks to Hisaji Ono. This style will subsample by default for more than 3000 observations. This style should always be preferred to "jenks" as it uses the original Fortran code and runs nested for-loops much faster.

  The "jenks" style has been ported from Jenks' code, and has been checked for consistency with ArcView, ArcGIS, and MapInfo (with some remaining differences); added here thanks to Hisaji Ono (originally reported as Basic, now seen as Fortran (as described in a talk last seen at http://www.irlogi.ie/wp-content/uploads/2016/11/NUIM_ChoroHarmful.pdf, slides 26-27)). Note that the sense of interval closure is reversed from the other styles, and in this implementation has to be right-closed - use cutlabels=TRUE in \code{findColours} on the object returned to show the closure clearly, and use \code{findCols} to extract the classes for each value. This style will subsample by default for more than 3000 observations.

  The "dpih" style uses the \code{dpih()} function from \pkg{KernSmooth} (Wand, 1995) implementing direct plug-in methodology to select the bin width of a histogram.

  The "headtails" style uses the algorithm proposed by Bin Jiang (2013), in order to find groupings or hierarchy for data with a heavy-tailed distribution. This classification scheme partitions all of the data values around the mean into two parts and continues the process iteratively for the values (above the mean) in the head until the head part values are no longer heavy-tailed distributed. Thus, the number of classes and the class intervals are both naturally determined. By default the algorithm uses \code{thr = 0.4}, meaning that when the head represents more than 40\% of the observations the distribution is not considered heavy-tailed. The threshold argument \code{thr} may be modified through \code{\dots} (see Examples).

  The "maximum" style uses the Maximum Breaks method of classification finding the k - 1 largest differences in \code{var}. The mean of the values that generated the largest splits is used as the interval boundary.
  
  The "box" style generates 7 breaks (therefore 6 categories) based on a box-and-whisker plot. First and last categories include the data values considered as outliers, and the four remaining categories are defined by the percentiles 25, 50 and 75 of the data distribution. By default, the identification of outliers is based on the interquantile range (IQR), so values lower than percentile 25 - 1.5 * IQR or higher than percentile 75 + 1.5 * IQR are considered as outliers. The multiplier applied to the IQR \code{iqr_mult = 1.5} may be modified through \code{\dots}; the value must not be negative. As in the \code{"quantile"} style, the \code{type=} argument may be used to choose the quantile algoritm (default 7, standard boxplots use 5 or 2). From 0.4-9 and #41, the maximum and minimum are set to \code{+Inf} and \code{-Inf} to avoid errors induced in the earlier version where breaks could cease to be strictly ascending. The \code{legacy=} argument with value \code{TRUE} may be used to revert to the previous behaviour.

}

\value{
 an object of class "classIntervals":
  \item{var}{the input variable}
  \item{brks}{a vector of breaks}
  and attributes:
  \item{style}{the style used}
  \item{parameters}{parameter values used in finding breaks}
  \item{nobs}{number of different finite values in the input variable}
  \item{call}{this function's call}
  \item{intervalClosure}{string, whether closure is \dQuote{left} or \dQuote{right}}
  \item{dataPrecision}{the data precision used for printing interval values in the legend returned by \code{findColours}, and in the \code{print} method for classIntervals objects. If intervalClosure is \dQuote{left}, the value returned is \code{ceiling} of the data value multiplied by 10 to the dataPrecision power, divided by 10 to the dataPrecision power.}
}
\references{
Armstrong, M. P., Xiao, N., Bennett, D. A., 2003. "Using genetic algorithms to create multicriteria class intervals for choropleth maps". Annals, Association of American Geographers, 93 (3), 595--623;

Jenks, G. F., Caspall, F. C., 1971. "Error on choroplethic maps: definition, measurement, reduction". Annals, Association of American Geographers, 61 (2), 217--244;

Dent, B. D., 1999, Cartography: thematic map design. McGraw-Hill, Boston, 417 pp.;

Slocum TA, McMaster RB, Kessler FC, Howard HH 2005 Thematic Cartography and Geographic Visualization, Prentice Hall, Upper Saddle River NJ.;

Fisher, W. D. 1958 "On grouping for maximum homogeneity", Journal of the American Statistical Association, 53, pp. 789--798 (\url{https://lib.stat.cmu.edu/cmlib/src/cluster/fish.f})

Wand, M. P. 1995. Data-based choice of histogram binwidth. The American Statistician, 51, 59-64.

Jiang, B. 2013 "Head/tail breaks: A new classification scheme for data with a heavy-tailed distribution", The Professional Geographer, 65 (3), 482 – 494. (\url{https://arxiv.org/abs/1209.2801v1})

}

\author{Roger Bivand <Roger.Bivand@nhh.no>}

\note{From version 0.1-11, the default representation has been changed to use \code{cutlabels=TRUE}, and representation within intervals has been corrected, thanks to Richard Dunlap. From version 0.1-15, the print method drops the calculation of the possible number of combinations of observations into classes, which generated warnings for n > 170.}

\seealso{\code{\link{findColours}}, \code{\link{findCols}}, \code{\link{pretty}}, \code{\link[stats]{quantile}}, \code{\link[stats]{kmeans}}, \code{\link[stats]{hclust}}, \code{\link[e1071]{bclust}}, \code{\link{findInterval}}, \code{\link[grDevices]{colorRamp}}, \code{\link[grDevices]{nclass}}, \code{\link[lattice]{shingle}}}
\examples{
if (!require("spData", quietly=TRUE)) {
  message("spData package needed for examples")
  run <- FALSE
} else {
  run <- TRUE
}
if (run) {
data(jenks71, package="spData")
pal1 <- c("wheat1", "red3")
opar <- par(mfrow=c(2,3))
plot(classIntervals(jenks71$jenks71, n=5, style="fixed",
 fixedBreaks=c(15.57, 25, 50, 75, 100, 155.30)), pal=pal1, main="Fixed")
plot(classIntervals(jenks71$jenks71, n=5, style="sd"), pal=pal1, main="Pretty standard deviations")
plot(classIntervals(jenks71$jenks71, n=5, style="equal"), pal=pal1, main="Equal intervals")
plot(classIntervals(jenks71$jenks71, n=5, style="quantile"), pal=pal1, main="Quantile")
set.seed(1)
plot(classIntervals(jenks71$jenks71, n=5, style="kmeans"), pal=pal1, main="K-means")
plot(classIntervals(jenks71$jenks71, n=5, style="hclust", method="complete"),
 pal=pal1, main="Complete cluster")
}
if (run) {
plot(classIntervals(jenks71$jenks71, n=5, style="hclust", method="single"),
 pal=pal1, main="Single cluster")
set.seed(1)
plot(classIntervals(jenks71$jenks71, n=5, style="bclust", verbose=FALSE),
 pal=pal1, main="Bagged cluster")
plot(classIntervals(jenks71$jenks71, n=5, style="fisher"), pal=pal1,
 main="Fisher's method")
plot(classIntervals(jenks71$jenks71, n=5, style="jenks"), pal=pal1,
 main="Jenks' method")
 plot(classIntervals(jenks71$jenks71, style="dpih"), pal=pal1,
 main="dpih method")
 plot(classIntervals(jenks71$jenks71, style="headtails", thr = 1), pal=pal1,
 main="Head Tails method")
 }
if (run) {
 plot(classIntervals(jenks71$jenks71, style="maximum"), pal=pal1,
 main="Maximum method")
 plot(classIntervals(jenks71$jenks71, style="box"), pal=pal1,
 main="Box method")
 par(opar)
}
if (run) {
print(classIntervals(jenks71$jenks71, n=5, style="fixed",
 fixedBreaks=c(15.57, 25, 50, 75, 100, 155.30)))
}
if (run) {
print(classIntervals(jenks71$jenks71, n=5, style="sd"))
}
if (run) {
print(classIntervals(jenks71$jenks71, n=5, style="equal"))
}
if (run) {
print(classIntervals(jenks71$jenks71, n=5, style="quantile"))
}
if (run) {
set.seed(1)
print(classIntervals(jenks71$jenks71, n=5, style="kmeans"))
}
if (run) {
set.seed(1)
print(classIntervals(jenks71$jenks71, n=5, style="kmeans", intervalClosure="right"))
}
if (run) {
set.seed(1)
print(classIntervals(jenks71$jenks71, n=5, style="kmeans", dataPrecision=0))
}
if (run) {
set.seed(1)
print(classIntervals(jenks71$jenks71, n=5, style="kmeans"), cutlabels=FALSE)
}
if (run) {
print(classIntervals(jenks71$jenks71, n=5, style="hclust", method="complete"))
}
if (run) {
print(classIntervals(jenks71$jenks71, n=5, style="hclust", method="single"))
}
if (run) {
set.seed(1)
print(classIntervals(jenks71$jenks71, n=5, style="bclust", verbose=FALSE))
}
if (run) {
print(classIntervals(jenks71$jenks71, n=5, style="bclust",
 hclust.method="complete", verbose=FALSE))
}
if (run) {
print(classIntervals(jenks71$jenks71, n=5, style="fisher"))
}
if (run) {
print(classIntervals(jenks71$jenks71, n=5, style="jenks"))
}
if (run) {
print(classIntervals(jenks71$jenks71, style="dpih"))
}
if (run) {
print(classIntervals(jenks71$jenks71, style="dpih", range.x=c(0, 160)))
}
if (run) {
  print(classIntervals(jenks71$jenks71, style="headtails"))
}
if (run) {
  print(classIntervals(jenks71$jenks71, style="headtails", thr = .45))
}
if (run) {
  print(classIntervals(jenks71$jenks71, style="maximum"))
}
if (run) {
  print(classIntervals(jenks71$jenks71, style="box"))
}
if (run) {
  print(classIntervals(jenks71$jenks71, style="box", iqr_mult = 0.25))
}
x <- c(0, 0, 0, 1, 2, 50)
print(classIntervals(x, n=3, style="fisher"))
print(classIntervals(x, n=3, style="jenks"))

# Argument 'unique' will collapse the label of classes containing a
# single value. This is particularly useful for 'censored' variables
# that contain for example many zeros.

data_censored<-c(rep(0,10), rnorm(100, mean=20,sd=1),rep(26,10))
plot(density(data_censored))
cl2 <- classIntervals(data_censored, n=5, style="jenks", dataPrecision=2)
print(cl2, unique=FALSE)
print(cl2, unique=TRUE)

\dontrun{
set.seed(1)
n <- 1e+05
x <- runif(n)
classIntervals(x, n=5, style="sd")
classIntervals(x, n=5, style="pretty")
classIntervals(x, n=5, style="equal")
classIntervals(x, n=5, style="quantile")
# the class intervals found vary a little because of sampling
classIntervals(x, n=5, style="kmeans")
classIntervals(x, n=5, style="fisher")
classIntervals(x, n=5, style="fisher")
classIntervals(x, n=5, style="fisher")
}
have_units <- FALSE
if (require(units, quietly=TRUE)) have_units <- TRUE
if (have_units) {
set.seed(1)
x_units <- set_units(sample(seq(1, 100, 0.25), 100), km/h)
\dontrun{
classIntervals(x_units, n=5, style="sd")
}
}
if (have_units) {
classIntervals(x_units, n=5, style="pretty")
}
if (have_units) {
\dontrun{
classIntervals(x_units, n=5, style="equal")
}
}
if (have_units) {
classIntervals(x_units, n=5, style="quantile")
}
if (have_units) {
\dontrun{
classIntervals(x_units, n=5, style="kmeans")
}
}
if (have_units) {
classIntervals(x_units, n=5, style="fisher")
}
if (have_units) {
classIntervals(x_units, style="headtails")
}
if (have_units) {
classIntervals(x_units, style="box")
}
\dontrun{
st <- Sys.time()
x_POSIXt <- sample(st+((0:500)*3600), 100)
fx <- st+((0:5)*3600)*100
classIntervals(x_POSIXt, style="fixed", fixedBreaks=fx)
classIntervals(x_POSIXt, n=5, style="sd")
classIntervals(x_POSIXt, n=5, style="pretty")
classIntervals(x_POSIXt, n=5, style="equal")
classIntervals(x_POSIXt, n=5, style="quantile")
classIntervals(x_POSIXt, n=5, style="kmeans")
classIntervals(x_POSIXt, n=5, style="fisher")
classIntervals(x_POSIXt, style="headtails")
classIntervals(x_POSIXt, style="maximum")
classIntervals(x_POSIXt, style="box")
}
# see vignette for further details
\dontrun{
# Head Tails method is suitable for right-sided heavy-tailed distributions
set.seed(1234)
# Heavy tails-----
# Pareto distributions a=7 b=14
paretodist <- 7 / (1 - runif(100)) ^ (1 / 14)
# Lognorm
lognormdist <- rlnorm(100)
# Weibull
weibulldist <- rweibull(100, 1, scale = 5)

pal1 <- c("wheat1", "red3")
opar <- par(mfrow = c(1, 3))
plot(classIntervals(paretodist, style = "headtails"),
     pal = pal1,
     main = "HeadTails: Pareto Dist.")
plot(classIntervals(lognormdist, style = "headtails"),
     pal = pal1,
     main = "HeadTails: LogNormal Dist.")
plot(classIntervals(weibulldist, style = "headtails"),
     pal = pal1,
     main = "HeadTails: Weibull Dist.")
plot(classIntervals(paretodist, n = 5, style = "fisher"),
     pal = pal1,
     main = "Fisher: Pareto Dist.")
plot(classIntervals(lognormdist, n = 7, style = "fisher"),
     pal = pal1,
     main = "Fisher: LogNormal Dist.")
plot(classIntervals(weibulldist, n= 4, style = "fisher"),
     pal = pal1,
     main = "Fisher: Weibull Dist.")
par(opar)


#Non heavy tails, thr should be increased-----

#Normal dist
normdist <- rnorm(100)
#Left-tailed truncated Normal distr
leftnorm <- rep(normdist[normdist < mean(normdist)], 2)
# Uniform distribution
unifdist <- runif(100)
opar <- par(mfrow = c(2, 3))
plot(classIntervals(normdist, style = "headtails"),
     pal = pal1,
     main = "Normal Dist.")
plot(classIntervals(leftnorm, style = "headtails"),
     pal = pal1,
     main = "Truncated Normal Dist.")
plot(classIntervals(unifdist, style = "headtails"),
     pal = pal1,
     main = "Uniform Dist.")
# thr should be increased for non heavy-tailed distributions
plot(
  classIntervals(normdist, style = "headtails", thr = .6),
  pal = pal1,
  main = "Normal Dist. thr = .6"
)
plot(
  classIntervals(leftnorm, style = "headtails", thr = .6),
  pal = pal1,
  main = "Truncated Normal Distribution thr = .6"
)
plot(
  classIntervals(unifdist, style = "headtails", thr = .6),
  pal = pal1,
  main = "Uniform Distribution thr = .6"
)
par(opar)
}
}
\keyword{spatial}

