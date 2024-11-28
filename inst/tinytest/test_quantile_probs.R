library(classInt)
# issue #1
set.seed(101)
data_censored <- c(rep(0,10), rnorm(100, mean=20,sd=1),rep(26,10))
expect_silent(classInt::classIntervals(data_censored, style = "quantile", 
  probs = seq(0, 1, 0.25)))
expect_error(classInt::classIntervals(data_censored, style = "quantile", 
  n = 5, probs = seq(0, 1, 0.25)))
expect_silent(classInt::classIntervals(data_censored, style = "quantile", 
  n = 4, probs = seq(0, 1, 0.25)))
expect_error(classInt::classIntervals(data_censored, style = "quantile", 
  probs = seq(-0.25, 1.25, 0.25)))
expect_warning(classInt::classIntervals(data_censored, style = "quantile", 
  probs = seq(0.25, 0.75, 0.25)))
expect_error(classInt::classIntervals(data_censored, style = "quantile", 
  probs = seq(0, 1, 0.25)-0.25))
expect_warning(classInt::classIntervals(data_censored, style = "quantile", 
  probs = c(0, 0.15804, 0.36603, 0.63975, 1))) # log sequence (bigsnpr::seq_log(1, 3, 5)-1)/2
