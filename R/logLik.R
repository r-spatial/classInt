#' Log-likelihood for classIntervals objects
#' 
#' @param object A classIntervals object
#' @param ... Ignored.
#' @return A `logLik` object (see `stats::logLik`).
#' @examples
#' x <- classIntervals(rnorm(100), n=5, style="fisher")
#' logLik(x)
#' AIC(x) # By having a logLik method, AIC.default is used.
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
  structure(current_loglik, df=df, nobs=attr(object, "nobs"), class="logLik")
}
