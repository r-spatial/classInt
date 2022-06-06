# 2022-06-06 Josiah Parry
# Wrapper function to return intervals as either integer or factor. 
# Executes findCols(classIntervals(...), factor = factor)
classify_intervals <- function(var, n, style = "quantile", rtimes = 3, ...,
                               intervalClosure = c("left", "right"), dataPrecision = NULL,
                               warnSmallN = TRUE, warnLargeN = TRUE, largeN = 3000L, 
                               samp_prop = 0.1,
                               gr = c("[", "]"),
                               factor = TRUE) {
  clI <- classIntervals(
    var = var,
    n = n, 
    style = style,
    rtimes = rtimes, 
    ...,
    intervalClosure = intervalClosure,
    dataPrecision = dataPrecision,
    warnSmallN = warnSmallN,
    warnLargeN = warnLargeN, 
    largeN = largeN,
    samp_prop = samp_prop,
    gr = gr)
  
  findCols(clI, factor = factor)
}

               
