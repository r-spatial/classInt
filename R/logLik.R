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
