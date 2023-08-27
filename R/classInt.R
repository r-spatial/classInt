gvf <- function(var, cols) {
   sumsq <- function(x) sum((x - mean(x))^2)
   sdam <- sumsq(var)
   sdcm <- sum(tapply(var, factor(cols), sumsq))
   res <- 1 - (sdcm/sdam)
   res
}

tai <- function(var, cols) {
   sumabs <- function(x) sum(abs(x - mean(x)))
   x <- sumabs(var)
   y <- sum(tapply(var, factor(cols), sumabs))
   res <- 1 - (y/x)
   res
}

oai <- function(var, cols, area) {
   sumabs1 <- function(x) sum(abs(x[,1] - mean(x[,1]))*x[,2])
   m <- cbind(as.numeric(var), as.numeric(area))
   x <- sumabs1(m)
   y <- sum(by(m, factor(cols), sumabs1))
   res <- 1 - (y/x)
   res
}

jenks.tests <- function(clI, area) {
   if (!inherits(clI, "classIntervals")) stop("Class interval object required")
   cols <- findCols(clI)
   res <- c("# classes"=length(clI$brks)-1,
     "Goodness of fit"=gvf(clI$var, cols),
     "Tabular accuracy"=tai(clI$var, cols))
   if (!missing(area)) {
      if (length(area) != length(cols))
         stop("area and classified variable different lengths")
      res <- c(res, "Overview accuracy"=oai(clI$var, cols, area))
   }
   res
}

plot.classIntervals <- function(x, pal, ...) {
   if (!inherits(x, "classIntervals")) stop("Class interval object required")
   if (length(pal) < 2) stop("pal must contain at least two colours")
   pal_out <- colorRampPalette(pal)(length(x$brks)-1)
   plot(ecdf(x$var), ...)
   stbrks <- cbind(x$brks[-length(x$brks)], x$brks[-1])
   abline(v=x$brks, col="grey")
   for (i in 1:nrow(stbrks))
      rect(stbrks[i,1], par("usr")[3], stbrks[i,2], 0, col=pal_out[i],
        border="transparent")
}

classIntervals2shingle <- function(x) {
	res <- x$var
	nl <- length(x$brks) - 1
	lres <- vector(mode="list", length=nl)
	for (i in 1:nl) lres[[i]] <- x$brks[c(i, i+1)]
	class(lres) <- "shingleLevel"
	attr(res, "levels") <- lres
	class(res) <- "shingle"
	res
}


# change contributed by Richard Dunlap 090512
# Added intervalClosure argument to allow specification of whether
# partition intervals are closed on the left or the right
# Added dataPrecision argument to allow rounding of interval boundaries
# to the precision -- the argument equals the number of
# decimal places in the data.  Negative numbers retain the usual
# convention for rounding.
classIntervals <- function(var, n, style="quantile", rtimes=3, ..., intervalClosure=c("left", "right"), dataPrecision=NULL, warnSmallN=TRUE, warnLargeN = TRUE, largeN = 3000L, samp_prop = 0.1, gr=c("[", "]")) {
  if (is.factor(var)) stop("var is categorical")
# https://github.com/r-spatial/classInt/issues/8
  TZ <- NULL
  POSIX <- FALSE
  DATE <- FALSE
  if (!is.numeric(var)) {
    if (inherits(var, "POSIXt")) {
      TZ <- attr(var, "tzone")
      POSIX <- TRUE
      var <- unclass(as.POSIXct(var))
    } else if (inherits(var, "Date")) {
	  var <- unclass(var)
	  DATE <- TRUE
	} else {
      stop("var is not numeric")
    }
  }
  UNITS <- NULL
  if (inherits(var, "units")) {
    UNITS <- paste0(gr[1], as.character(attr(var, "units")), gr[2])
  }
# Matthieu Stigler 120705
  intervalClosure <- match.arg(intervalClosure)
  ovar <- var
  if (length(style) > 1L) style <- style[1L]
  if (any(is.na(var))) {
    warning("var has missing values, omitted in finding classes")
    var <- c(na.omit(var))
  }
  if (any(!is.finite(var))) {
    warning("var has infinite values, omitted in finding classes")
    is.na(var) <- !is.finite(var)
  }
  nobs <- length(unique(var))
  if (nobs == 1) stop("single unique value")
  # Fix 22: Diego Hernangómez
  needn <- !(style %in% c("dpih", "headtails", "box"))

  if (missing(n)) n <- nclass.Sturges(var)
  if (n < 2 & needn) stop("n less than 2")
  n <- as.integer(n)
  pars <- NULL
  if (n > nobs & needn) {
    if (warnSmallN) {
      warning(paste("n greater than number of different finite values",
      "n reset to number of different finite values", sep="\\n"))
    }
    n <- nobs
  }
  if (n == nobs & needn) {
    if (warnSmallN) {
      warning(paste("n same as number of different finite values",
      "each different finite value is a separate class", sep="\\n"))
    }
    sVar <- sort(unique(var))
    dsVar <- diff(sVar)
    brks <- c(sVar[1]-(mean(dsVar)/2), sVar[1:(length(sVar)-1)]+(dsVar/2),
      sVar[length(sVar)]+(mean(dsVar)/2))
    style="unique"
  } else {
# introduced related to https://github.com/r-spatial/classInt/issues/7
    sampling <- FALSE
    if (warnLargeN &&
      (style %in% c("fisher", "jenks"))) {
      if (nobs > largeN) {
        warning("N is large, and some styles will run very slowly; sampling imposed")
        sampling <- TRUE
# issue 44
        nsamp <- as.integer(ceiling(samp_prop*nobs))
        if (nsamp > largeN) nsamp <- as.integer(largeN)
      }
    }
    if (style =="fixed") {
#      mc <- match.call(expand.dots=FALSE)
#      fixedBreaks <- sort(eval(mc$...$fixedBreaks))
# Matthieu Stigler 111110
      dots <- list(...)
      fixedBreaks <- sort(dots$fixedBreaks)
      if (is.null(fixedBreaks))
        stop("fixed method requires fixedBreaks argument")
#      if (length(fixedBreaks) != (n+1))
#        stop("mismatch between fixedBreaks and n")
      if (!is.numeric(fixedBreaks)) {
# fixedBreaks assumed to be TZ-compliant with var
        if (inherits(fixedBreaks, "POSIXt") && POSIX) {
          fixedBreaks <- unclass(as.POSIXct(fixedBreaks))
        } else if (inherits(fixedBreaks, "DATE") && DATE) {
          fixedBreaks <- unclass(fixedBreaks)
        } else {
          stop("fixedBreaks must be numeric")
        }
      }
      if (any(diff(fixedBreaks) < 0)) stop("decreasing fixedBreaks found")
      if (min(var) < fixedBreaks[1] ||
        max(var) > fixedBreaks[length(fixedBreaks)])
          warning("variable range greater than fixedBreaks")
      brks <- fixedBreaks
    } else if (style =="sd") {
      svar <- scale(var)
      pars <- c(attr(svar, "scaled:center"), attr(svar, "scaled:scale"))
      names(pars) <- c("center", "scale")
      sbrks <- pretty(x=svar, n=n, ...)
      brks <- c((sbrks * pars[2]) + pars[1])
    } else if (style =="equal") {
      brks <- seq(min(var), max(var), length.out=(n+1))
    } else if (style =="pretty") {
      brks <- c(pretty(x=var, n=n, ...))
    } else if (style =="quantile") {
# stats
      brks <- c(quantile(x=var, probs=seq(0,1,1/n), ...))
      names(brks) <- NULL
    } else if (style =="kmeans") {
# stats
      pars <- try(kmeans(x=var, centers=n, ...))
      if (inherits(pars, "try-error")) {
        warning("jittering in kmeans")
        jvar <- jitter(rep(x=var, times=rtimes))
        pars <- try(kmeans(x=jvar, centers=n, ...))
	if (inherits(pars, "try-error")) stop("kmeans failed after jittering")
        else {
          cols <- match(pars$cluster, order(c(pars$centers)))
          rbrks <- unlist(tapply(jvar, factor(cols), range))
        }
      } else {
        cols <- match(pars$cluster, order(c(pars$centers)))
        rbrks <- unlist(tapply(var, factor(cols), range))
      }
      names(rbrks) <- NULL
      brks <- .rbrks(rbrks)
    } else if (style =="hclust") {
# stats
      pars <- hclust(dist(x=var, method="euclidean"), ...)
      rcluster <- cutree(tree=pars, k=n)
      rcenters <- unlist(tapply(var, factor(rcluster), mean))
      cols <- match(rcluster, order(c(rcenters)))
      rbrks <- unlist(tapply(var, factor(cols), range))
      names(rbrks) <- NULL
      brks <- .rbrks(rbrks)
    } else if (style =="bclust") {
# e1071, class
      pars <- try(bclust(x=var, centers=n, ...))
      if (inherits(pars, "try-error")) {
        warning("jittering in bclust")
        jvar <- jitter(rep(x=var, times=rtimes))
        pars <- try(bclust(x=jvar, centers=n, ...))
	if (inherits(pars, "try-error")) stop("bclust failed after jittering")
        else {
          cols <- match(pars$cluster, order(c(pars$centers)))
          rbrks <- unlist(tapply(jvar, factor(cols), range))
        }
      } else {
        cols <- match(pars$cluster, order(c(pars$centers)))
        rbrks <- unlist(tapply(var, factor(cols), range))
      }
      names(rbrks) <- NULL
      brks <- .rbrks(rbrks)
    } else if (style =="fisher") {
# introduced related to https://github.com/r-spatial/classInt/issues/7
      if (sampling) {
        pars <- fish(x=c(range(var), sample(x=var, size=nsamp)), k=n)
      } else {
        pars <- fish(x=var, k=n)
      }
      brks <- pars[n,1]
      for (i in n:1) brks <- c(brks, (pars[i,2]+pars[(i-1),1])/2)
      brks <- c(brks, pars[1,2])
      colnames(pars) <- c("min", "max", "class mean", "class sd")
    } else if (style == "jenks") { # Jenks Optimisation Method
# change contributed by Richard Dunlap 090512
# This version of the Jenks code assumes intervals are closed on
# the right -- force it.
    	   intervalClosure = "right"
           if (storage.mode(var) != "double") storage.mode(var) <- "double"
# introduced related to https://github.com/r-spatial/classInt/issues/7
           if (sampling) {
             message("Use \"fisher\" instead of \"jenks\" for larger data sets")
             d <- sort(c(range(var), sample(x=var, size=nsamp)))
           } else {
             d <- sort(var)
           }
           k <- n
           #work<-matrix(0,k,length(d))
           mat1 <- matrix(1, length(d), k)
           mat2 <- matrix(0, length(d), k)
           mat2[2:length(d),1:k] <- .Machine$double.xmax #R's max double value?
           v<-0

           for(l in 2:length(d)){
             s1=s2=w=0
             for(m in 1:l){
               i3 <- l - m + 1
               val <- d[i3]
               s2 <- s2 + val * val
               s1 <- s1 + val
               w<-w+1
               v <- s2 - (s1 * s1) / w
               i4 <- trunc(i3 - 1)

               if(i4 !=0){
                 for(j in 2:k){
                   if(mat2[l,j] >= (v + mat2[i4, j - 1])){
                     mat1[l,j] <- i3
                     mat2[l,j] <- v + mat2[i4, j - 1]
                   }
                 }
               }
             }
             mat1[l,1] <- 1
             mat2[l,1] <- v
           }

           kclass<-1:k
           kclass[k] <- length(d)
           k <- length(d)
           last<-length(d)
           for(j in length(kclass):1){
             id <- trunc(mat1[k,j]) - 1
             kclass[j - 1] <- id
             k <- id #lower
             last <- k -1 #upper
           }
# change uncontributed by Richard Dunlap 090512
# with the specification of intervalClosure for the presentation layer,
# don't need to change this
           brks<-d[c(1, kclass)]

      } else if (style == "dpih") {
# introduced related to https://github.com/r-spatial/classInt/issues/6
           h <- dpih(var, ...)
           dots <- list(...)
           if (!is.null(dots$range.x)) {
               vmin <- dots$range.x[1]
               vmax <- dots$range.x[2]
           } else {
               vmin <- min(var)
               vmax <- max(var)
           }
           brks <- seq(vmin, vmax, by=h)
      } else if (style == "headtails") {
             # Contributed Diego Hernangómez
# https://github.com/mtennekes/tmap/issues/555
             dots <- list(...)
             thr <- ifelse(is.null(dots$thr),
                           .4,
                           dots$thr)

             thr <-  min(1,max(0, thr))
             head <- var
             breaks <- min(var, na.rm = TRUE) #Init with minimum
             for (i in 1:100) {
               mu <- mean(head, na.rm = TRUE)
               breaks <- c(breaks, mu)
               ntot <- length(head)
               #Switch head
               head <- head[head > mu]
               prop <- length(head) / ntot
               keepiter <- prop <= thr & length(head) > 1
               if (isFALSE(keepiter)) {break}
             }
             #Add max to complete intervals
             brks <- sort(unique(c(breaks,
                                   max(var, na.rm = TRUE))))
      } else if (style == "maximum") {

        # 2022-06-05 Josiah Parry
        x_sort <- sort(var)
        diffs <- diff(x_sort)

        n_breaks <- sort(diffs, decreasing = TRUE)[1:(n-1)]

        # identify values to average for breaks
        int_end_index <- which(diffs %in% n_breaks)
        int_nb_index <- which(diffs %in% n_breaks) + 1

        m <- matrix(
          c(x_sort[int_nb_index], x_sort[int_end_index]),
          ncol = 2
        )

        brks <- c(min(x_sort), rowSums(m) / 2, max(x_sort))


      } else if (style == "box"){
        # 2022-09-22 Diego Hernangomez, see:
        # https://github.com/r-spatial/classInt/issues/18
        # Adapted from:
        # https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html#box-map
        
        dots <- list(...)
        iqr_mult <- ifelse(is.null(dots$iqr_mult), 1.5, dots$iqr_mult)
        stopifnot(iqr_mult >= 0)
        qtype <- ifelse(is.null(dots$type), 7, dots$type)
        legacy <- ifelse(is.null(dots$legacy), FALSE, dots$legacy)
        
        qv <- unname(quantile(var, type=qtype))
        iqr <- iqr_mult * (qv[4] - qv[2])
        upfence <- qv[4] + iqr
        lofence <- qv[2] - iqr
        
        # initialize break points vector
        bb <- vector(mode="numeric",length=7)
        
        # logic for lower and upper fences
        if (lofence < qv[1]) {  # no lower outliers
          if (legacy) {
            bb[1] <- lofence
            bb[2] <- floor(qv[1])
          } else {
            bb[1] <- -Inf
            bb[2] <- lofence
          }
        } else {
          bb[2] <- lofence
          bb[1] <- qv[1]
        }
        if (upfence > qv[5]) { # no upper outliers
          if (legacy) {
            bb[7] <- upfence
            bb[6] <- ceiling(qv[5])
          } else {
            bb[7] <- +Inf
            bb[6] <- upfence
          }
        } else {
          bb[6] <- upfence
          bb[7] <- qv[5]
        }
        bb[3:5] <- qv[2:4]
        
        brks <- bb
        
      } else stop(paste(style, "unknown"))
  }
  if (is.null(brks)) stop("Null breaks")
  if (POSIX) {
    ovar <- .POSIXct(ovar, TZ)
    brks <- .POSIXct(brks, TZ)
  } else if (DATE) {
    ovar <- as.Date(ovar, origin = "1970-01-01")
    brks <- as.Date(brks, origin = "1970-01-01")
  }
  res <- list(var=ovar, brks=brks)
  attr(res, "style") <- style
  attr(res, "parameters") <- pars
  attr(res, "nobs") <- nobs
  attr(res, "call") <- match.call()
# change contributed by Richard Dunlap 090512
# Add intervalClosure and dataPrecision to the attributes so they're
# available to the print method
  attr(res, "intervalClosure") <- intervalClosure
  attr(res, "dataPrecision") <- dataPrecision
  attr(res, "var_units") <- UNITS
  class(res) <- "classIntervals"
  res
}

.rbrks <- function(rbrks) {
  nb <- length(rbrks)
  if (nb < 2) stop("single break")
  brks <- c(rbrks[1], rbrks[nb])
  if (nb > 2) {
    if (nb == 3) brks <- append(brks, rbrks[2], 1)
    else {
      ins <- NULL
      for (i in as.integer(seq(2,(nb-2),2))) {
        ins <- c(ins, ((rbrks[i]+rbrks[i+1])/2))
      }
      brks <- append(brks, ins, 1)
    }
  }
  brks
}

findColours <- function(clI, pal, under="under", over="over", between="-",
  digits = getOption("digits"), cutlabels=TRUE) {
  if (!inherits(clI, "classIntervals")) stop("Class interval object required")
  if (is.null(clI$brks)) stop("Null breaks")
  if (length(pal) < 2) stop("pal must contain at least two colours")
  cols <- findCols(clI)
  palette <- colorRampPalette(pal)(length(clI$brks)-1)
  res <- palette[cols]
  attr(res, "palette") <- palette
  tab <- tableClassIntervals(cols=cols, brks=clI$brks, under=under, over=over,
    between=between, digits=digits, cutlabels=cutlabels,
    intervalClosure=attr(clI, "intervalClosure"),
    dataPrecision=attr(clI, "dataPrecision"))
  attr(res, "table") <- tab
  res
}

# change contributed by Richard Dunlap 090512
# Looks for intervalClosure attribute to allow specification of
# whether partition intervals are closed on the left or the right
findCols <- function(clI, factor = FALSE)  {
  if (!inherits(clI, "classIntervals")) stop("Class interval object required")
  if (is.null(clI$brks)) stop("Null breaks")
  if (is.null(attr(clI, "intervalClosure")) || (attr(clI, "intervalClosure") == "left")) {
  	cols <- findInterval(clI$var, clI$brks, all.inside=TRUE)
  }
  else {
	cols <- apply(array(apply(outer(clI$var, clI$brks, ">"), 1, sum)), 1, max, 1)
  }
  if (factor) {
    col_vals <- names(tableClassIntervals(cols, clI$brks))
    cols <- factor(cols, levels = 1:length(col_vals), labels = col_vals)
  }
  cols
}

# change contributed by Richard Dunlap 090512
# Added intervalClosure argument to allow specification of whether
# partition intervals are closed on the left or the right
# Added dataPrecision for rounding of the interval endpoints
tableClassIntervals <- function(cols, brks, under="under", over="over",
   between="-", digits = getOption("digits"), cutlabels=TRUE, intervalClosure=c("left", "right"), dataPrecision=NULL, unique=FALSE, big.mark=NULL, var) {
# Matthieu Stigler 120705 unique
# Matthieu Stigler 120705
   intervalClosure <- match.arg(intervalClosure)
   lx <- length(brks)
   nres <- character(lx - 1)
   sep <- " "
   if (cutlabels) {
      sep <- ""
      between=","
   }

   if (is.null(intervalClosure) || (intervalClosure=="left")) {
   	left = "["
   	right = ")"
   }
   else {
   	left = "("
   	right = "]"
   }

#The two global endpoints are going through roundEndpoint to get
# formatting right, nothing more
   if (cutlabels) nres[1] <- paste("[", roundEndpoint(brks[1], intervalClosure, dataPrecision, big.mark), between, roundEndpoint(brks[2], intervalClosure, dataPrecision, big.mark), right, sep=sep)
   else nres[1] <- paste(under, roundEndpoint(brks[2], intervalClosure, dataPrecision), sep=sep)
   for (i in 2:(lx - 2)) {
      if (cutlabels) nres[i] <- paste(left, roundEndpoint(brks[i], intervalClosure, dataPrecision, big.mark), between, roundEndpoint(brks[i + 1], intervalClosure, dataPrecision, big.mark), right,
         sep=sep)
      else nres[i] <- paste(roundEndpoint(brks[i], intervalClosure, dataPrecision, big.mark), between, roundEndpoint(brks[i + 1], intervalClosure, dataPrecision, big.mark), sep=sep)
   }
   if (cutlabels) nres[lx - 1] <- paste(left, roundEndpoint(brks[lx - 1], intervalClosure, dataPrecision, big.mark), between, roundEndpoint(brks[lx], intervalClosure, dataPrecision, big.mark), "]",
     sep=sep)
   else nres[lx - 1] <- paste(over, roundEndpoint(brks[lx - 1], intervalClosure, dataPrecision, big.mark), sep=sep)
   tab <- table(factor(cols, levels=1:(lx - 1)))
   names(tab) <- nres

# Matthieu Stigler 120705 unique
   ## Assign unique label for intervals containing same left-right points
  if(unique&!missing(var)){

    tab_unique<-tapply(var, cols, function(x) length(unique(x)))
#    tab_unique_vals<-tapply(var, cols, function(x) length(unique(x)))
    if(any(tab_unique==1)){
#      w.unique <-which(tab_unique==1)
      w.unique <-as.numeric(names(which(tab_unique==1)))
      cat("Class found with one single (possibly repeated) value: changed label\n")
#      cols.unique <-cols%in%names(w.unique)
      cols.unique <-cols%in%w.unique
      names(tab)[w.unique] <- tapply(var[cols.unique ], cols[cols.unique ], function(x) if(is.null(dataPrecision)) unique(x) else round(unique(x), dataPrecision))
    }
  }

   tab
}

# change contributed by Richard Dunlap 090512
# New helper method for tableClassIntervals
roundEndpoint <- function(x, intervalClosure=c("left", "right"), dataPrecision, big.mark) {
# Matthieu Stigler 120705
  intervalClosure <- match.arg(intervalClosure)
   if (is.null(dataPrecision)) {
      retval <- x
   }
   else if (is.null(intervalClosure) || (intervalClosure=="left")) {
      retval <- ceiling(x * 10^dataPrecision) / 10^dataPrecision
   }
   else
   {
      retval <- floor(x * 10^dataPrecision) / 10^dataPrecision
   }
  
   if(!missing(big.mark)) if(is.character(big.mark)) {
     retval <- prettyNum(retval, big.mark = big.mark)
   }
  
   digits = getOption("digits")
   format(retval, digits=digits, trim=TRUE)
} #FIXME output trailing zeros in decimals

print.classIntervals <- function(x, digits = getOption("digits"), ..., under="under", over="over", between="-", cutlabels=TRUE, unique=FALSE, big.mark = NULL) {
   if (!inherits(x, "classIntervals")) stop("Class interval object required")
   cat("style: ", attr(x, "style"), "\n", sep="")
   UNITS <- attr(x, "var_units")
   if (is.null(UNITS)) UNITS <- ""
   else UNITS <- paste0(UNITS, " ")
   nP <- nPartitions(x)
   if (is.finite(nP)) cat("  one of ", prettyNum(nP, big.mark = ","),
      " possible partitions of this ", UNITS, "variable into ", length(x$brks)-1,
      " classes\n", sep="")
   cols <- findCols(x)
   nvar <- x$var
   if (inherits(nvar, "units")) attributes(nvar) <- NULL
   nbrks <- x$brks
   if (inherits(nbrks, "units")) attributes(nbrks) <- NULL
# change contributed by Richard Dunlap 090512
# passes the intervalClosure argument to tableClassIntervals
   tab <- tableClassIntervals(cols=cols, brks=nbrks, under=under, over=over,
    between=between, digits=digits, cutlabels=cutlabels, intervalClosure=attr(x, "intervalClosure"), dataPrecision=attr(x, "dataPrecision"), unique=unique, big.mark = big.mark, var = nvar)
   print(tab, digits=digits, ...)
   invisible(tab)
}

nPartitions <- function(x) {
  n <- attr(x, "nobs")
  if (n > 170) ret <- Inf
  else {
      k <- length(x$brks)-1
      ret <- (factorial(n - 1))/(factorial(n - k) * factorial(k - 1))
  }
  ret
}

getBclustClassIntervals <- function(clI, k) {
  if (!inherits(clI, "classIntervals")) stop("Class interval object required")
  if (missing(k)) k <- length(clI$brks)-1
  if (!inherits(attr(clI, "parameters"), "bclust"))
    stop("Class interval object not made with style=\"bclust\"")

  ovar <- clI$var
  var <- clI$var
  if (any(!is.finite(var))) is.na(var) <- !is.finite(var)
  var <- c(na.omit(var))

  obj <- attr(clI, "parameters")
  cols <- match(clusters.bclust(obj, k=k), order(centers.bclust(obj, k=k)))
  rbrks <- unlist(tapply(var, factor(cols), range))
  names(rbrks) <- NULL
  brks <- .rbrks(rbrks)

  res <- list(var=ovar, brks=brks)
  attr(res, "style") <- attr(clI, "style")
  attr(res, "parameters") <- attr(clI, "parameters")
  attr(res, "nobs") <- attr(clI, "nobs")
  attr(res, "call") <- attr(clI, "call")
  attr(res, "modified") <- c(attr(clI, "modified"), k)
  class(res) <- "classIntervals"
  res

}

getHclustClassIntervals <- function(clI, k) {
  if (!inherits(clI, "classIntervals")) stop("Class interval object required")
  if (missing(k)) k <- length(clI$brks)-1
  if (!inherits(attr(clI, "parameters"), "hclust"))
    stop("Class interval object not made with style=\"hclust\"")

  ovar <- clI$var
  var <- clI$var
  if (any(!is.finite(var))) is.na(var) <- !is.finite(var)
  var <- c(na.omit(var))

  obj <- attr(clI, "parameters")
  rcluster <- cutree(tree=obj, k=k)
  rcenters <- unlist(tapply(var, factor(rcluster), mean))
  cols <- match(rcluster, order(c(rcenters)))
  rbrks <- unlist(tapply(var, factor(cols), range))
  names(rbrks) <- NULL
  brks <- .rbrks(rbrks)

  res <- list(var=ovar, brks=brks)
  attr(res, "style") <- attr(clI, "style")
  attr(res, "parameters") <- attr(clI, "parameters")
  attr(res, "nobs") <- attr(clI, "nobs")
  attr(res, "call") <- attr(clI, "call")
  attr(res, "modified") <- c(attr(clI, "modified"), k)
  class(res) <- "classIntervals"
  res

}

fish <- function(x, k) {
   x <- sort(x)
   m <- length(x)
   k <- as.integer(k)
   work <- double(m*k)
   iwork <- integer(m*k)
   res <- double(k*4)
   out <- .Fortran("fish", as.integer(m), as.double(x), as.integer(k),
      as.integer(m), as.double(work), as.integer(m), as.integer(iwork),
      as.double(res), PACKAGE="classInt")[[8]]
   out <- matrix(out, k, 4)
   out
}
