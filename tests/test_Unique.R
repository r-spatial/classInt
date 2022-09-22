library(classInt)
set.seed(1)
data_censored<-c(rep(0,10), rnorm(100, mean=20,sd=1),rep(26,10))
cl2<-classIntervals(data_censored, n=4, style="fixed",dataPrecision=2,fixedBreaks=c(-1,1,19,25,30))

print(cl2, unique=FALSE)
print(cl2, unique=TRUE)

### example from man page
classIntervals(data_censored, n=5, style="fixed", fixedBreaks=c(15.57, 25, 50, 75, 100, 155.30))

print(classIntervals(data_censored, n=5, style="sd"), unique=FALSE)
print(classIntervals(data_censored, n=5, style="sd"), unique=TRUE)
print(classIntervals(data_censored, n=5, style="equal"),  unique=TRUE)
print(classIntervals(data_censored, n=5, style="quantile"),  unique=TRUE)
set.seed(1)
print(classIntervals(data_censored, n=5, style="kmeans"),  unique=TRUE)
print(classIntervals(data_censored, n=5, style="hclust", method="complete"),  unique=TRUE)
print(classIntervals(data_censored, n=5, style="hclust", method="single"),  unique=TRUE)
print(classIntervals(data_censored, n=5, style="fisher"),  unique=TRUE)
print(classIntervals(data_censored, n=5, style="jenks"),  unique=TRUE)

print(classIntervals(data_censored, n=5, style="fixed", fixedBreaks=c(15.57, 25, 50, 75, 100, 155.30)), unique=TRUE)
print(classIntervals(data_censored, n=5, style="sd"), unique=TRUE)
print(classIntervals(data_censored, n=5, style="equal"), unique=TRUE)
print(classIntervals(data_censored, n=5, style="quantile"), unique=TRUE)
set.seed(1)
print(classIntervals(data_censored, n=5, style="kmeans"), unique=TRUE)
set.seed(1)
print(classIntervals(data_censored, n=5, style="kmeans", intervalClosure="right"), unique=TRUE)
set.seed(1)
print(classIntervals(data_censored, n=5, style="kmeans", dataPrecision=0), unique=TRUE)
set.seed(1)
print(classIntervals(data_censored, n=5, style="kmeans"), cutlabels=FALSE, unique=TRUE)
print(classIntervals(data_censored, n=5, style="hclust", method="complete"), unique=TRUE)
print(classIntervals(data_censored, n=5, style="hclust", method="single"), unique=TRUE)
print(classIntervals(data_censored, n=5, style="fisher"), unique=TRUE)
print(classIntervals(data_censored, n=5, style="jenks"), unique=TRUE)
print(classIntervals(data_censored, style="headtails"), unique=TRUE)
print(classIntervals(data_censored, style="headtails", thr = 1))
print(classIntervals(data_censored, style="headtails", thr = 0))
print(classIntervals(data_censored, style="box", iqr_mult = 0))
print(classIntervals(data_censored, style="box"))
x <- c(0, 0, 0, 1, 2, 50)
print(classIntervals(x, n=3, style="fisher"), unique=TRUE)
print(classIntervals(x, n=3, style="jenks"), unique=TRUE)
if (getRversion() > "3.5.3") {
  suppressWarnings(set.seed(1, sample.kind=c("Rounding")))
} else {
  set.seed(1)
}
print(classIntervals(data_censored, n=5, style="bclust", verbose=FALSE),  unique=TRUE)
print(classIntervals(data_censored, n=5, style="bclust", hclust.method="complete", verbose=FALSE), unique=TRUE)

# the log-likelihood returns a valid logLik object.
stopifnot(
  identical(
    round(logLik(classIntervals(rep(1:3, each=10), n=2, style="jenks")), 5),
    structure(-14.52876, df = 2, nobs = 30L, class = "logLik")
  )
)
# logLik for exact intervals (a single value is the unique member of an
# interval) yields a likelihood of zero.
stopifnot(
  identical(
    suppressWarnings(logLik(classIntervals(rep(1:3, each=10), n=3, style="jenks"))),
    structure(0, df = 3, nobs = 30L, class = "logLik")
  )
)
