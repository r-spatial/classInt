#' Log-likelihood for classIntervals objects
#' 
#' @details 
#' 
#' Generally, the likelihood is a method for minimizing the standard deviation
#' within an interval, and with the AIC, a per-interval penalty can be used to
#' maximize the information and self-similarity of data in the interval.
#'
#' Based on Birge 2006 and Davies 2009 (see references), interval binning
#' selections may be compared by likelihood to optimize the number of intervals
#' selected for a set of data.  The `logLik()` function (and associated `AIC()`
#' function) can be used to optimize binning by maximizing the likelihood across
#' choices of intervals.
#' 
#' As illustrated by the examples below (the AIC comparison does not
#' specifically select 3 intervals when comparing 2, 3, and 4 intervals for data
#' with 3 intervals), while likelihood-based methods can provide evidence toward
#' optimization of binning, they are not infallible for bin selection.
#'
#' @param object A classIntervals object
#' @param ... Ignored.
#' @return A `logLik` object (see `stats::logLik`).
#' @examples
#' x <- classIntervals(rnorm(100), n=5, style="fisher")
#' logLik(x)
#' AIC(x) # By having a logLik method, AIC.default is used.
#' 
#' # When the intervals are made of a limited number of discrete values, the
#' # logLik is zero by definition (the standard deviation is zero giving a dirac
#' # function at the discrete value indicating a density of 1 and a log-density
#' # of zero).
#' x <- classIntervals(rep(1:2, each=10), n=2, style="jenks")
#' logLik(x)
#' x <- classIntervals(rep(1:3, each=10), n=2, style="jenks")
#' logLik(x)
#' 
#' # With slight jitter but notable categorical intervals (at 1, 2, and 3), the
#' # AIC will make selection of the optimal intervals easier.
#' data <- rep(1:3, each=100) + runif(n=300, min=-0.01, max=0.01)
#' x_2 <- classIntervals(data, n=2, style="jenks")
#' x_3 <- classIntervals(data, n=3, style="jenks")
#' x_4 <- classIntervals(data, n=4, style="jenks")
#' AIC(x_2, x_3, x_4)
#' @references
#' Lucien Birge, Yves Rozenholc.  How many bins should be put in a regular
#' histogram.  ESAIM: Probability and Statistics. 31 January 2006. 10:24-45.
#' url: https://www.esaim-ps.org/articles/ps/abs/2006/01/ps0322/ps0322.html.
#' doi:10.1051/ps:2006001
#' 
#' Laurie Davies, Ursula Gather, Dan Nordman, Henrike Weinert. A comparison of
#' automatic histogram constructions. ESAIM: Probability and Statistics. 	11
#' June 2009.  13:181-196. url:
#' https://www.esaim-ps.org/articles/ps/abs/2009/01/ps0721/ps0721.html
#' doi:10.1051/ps:2008005
#' @export
logLik.classIntervals <- function(object, ...) {
  df <- length(object$brks) - 1
  current_loglik <- 0
  for (idx in seq_len(df)) {
    mask_current <-
      if (((idx == 1) & (attr(object, "intervalClosure") == "right")) |
          ((idx == df) & (attr(object, "intervalClosure") == "left"))) {
        object$brks[idx] <= object$var &
          object$var <= object$brks[idx + 1]
      } else if (attr(object, "intervalClosure") == "right") {
        object$brks[idx] < object$var &
          object$var <= object$brks[idx + 1]
      } else if (attr(object, "intervalClosure") == "left") {
        object$brks[idx] <= object$var &
          object$var < object$brks[idx + 1]
      }
    if (sum(mask_current)) {
      current_x <- object$var[mask_current]
      current_loglik <-
        current_loglik +
        if (length(unique(current_x)) == 1) {
          # Assume that the density is 1 at the unique value's location and zero
          # elsewhere.  Therefore the log-density is 0.
          0
        } else {
          sum(dnorm(x=current_x, mean=mean(current_x), sd=sd(current_x), log=TRUE))
        }
    }
  }
  structure(current_loglik, df=df, nobs=length(object$var), class="logLik")
}
