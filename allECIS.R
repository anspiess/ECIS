library(minpack.lm)
library(segmented)
library(splines)
library(readxl)

getECIS <- function(file, freq = NULL) {
  ## get ECIS data
  IN <- as.data.frame(read_excel(file, col_names = FALSE))
  
  ## check for original format
  EP <- IN[1, ]; if (EP != "[ECIS_Parameters]") stop("Not an original ECIS file")
  
  ## if multiple frequencies, select the one given in input
  hasFreq <- grep("Frequency", IN[, 1])[-1]
  if (length(hasFreq) > 0) {
    Frequencies <- IN[hasFreq, 1]
    Frequencies <- as.numeric(sub("Frequency, ", "", Frequencies))
    if (!is.null(freq)) RL <- freq 
    else {
      cat("Available Frequencies [Hz]:", Frequencies, "\n")
      cat("Select one and type chosen frequency:")
      RL <- as.numeric(readLines(n = 1))
      while(!(RL %in% Frequencies)) {
        cat("This is not one of the available frequencies! Once more...\n")
        RL <- as.numeric(readLines(n = 1))
      }
    }
    sel <- which(Frequencies == RL)
    if (sel < length(Frequencies)) IN <- IN[(hasFreq[sel] + 1):(hasFreq[sel + 1] - 2), , drop = FALSE]
    else IN <- IN[(hasFreq[sel] + 1):nrow(IN), , drop = FALSE]
  } else Frequencies <- NULL
    
  ## find line containing header + well info
  sel <- grep("Time\\(hrs\\)", IN[, 1])
  Header <- strsplit(IN[sel, ], ",")[[1]]; Header <- sub(" ", "", Header)
  Wells <- strsplit(IN[sel+1, ], ",")[[1]]; Wells <- sub(" ", "", Wells)
  if (length(Header) != length(Wells)) stop("Header and Wells annotation of unequal length!")
  
  ## create new header info
  Header <- paste(Header, Wells, sep = "_")
  
  ## define data lines
  Seq <- (sel + 2):nrow(IN)
  
  ## preallocate empty matrix
  MAT <- matrix(NA_real_, nrow = length(Seq), ncol = length(Header))
  
  ## split ECIS data into matrix
  for (i in 1:length(Seq)) MAT[i, ] <- as.numeric(strsplit(IN[Seq[i], ], ",")[[1]])
  colnames(MAT) <- Header
 
  ## get rid of resistance and capacitance
  m1 <- grep("Res.", colnames(MAT)); m2 <- grep("Cap.", colnames(MAT)) 
  if (length(m1) != 0 & length(m2) != 0) MAT <- MAT[, -c(m1, m2)]
  
  ## split data into list of time/impedance
  splitVec <- split(1:ncol(MAT), rep(1:(ncol(MAT)/2), each = 2))
  LIST <- lapply(splitVec, function(x) MAT[, x])
  
  ## assign classes
  for (i in 1:length(LIST)) class(LIST[i]) <- c("matrix", "ecis")
  class(LIST) <- c("ecis", "list")
  return(LIST)
}

plotECIS <- function(ecis, which = NULL, style = c("matrix", "single"), fit = c("none", "spl", "log", "seg"), col = NULL, legend = TRUE, cex.legend = 1, ...) 
{
  style <- match.arg(style); fit <- match.arg(fit)
  if (!is.null(dev.list())) dev.off()
  
  ## select data from 'fit' or 'ecis' object
  if (class(ecis)[2] == "fit") {
    Data <- lapply(ecis, function(x) x$data) 
    if (fit == "spl") Preds <- lapply(ecis, function(x) x$pred.spline) 
    else if (fit == "log") Preds <- lapply(ecis, function(x) x$pred.log)
    else if (fit == "seg") Preds <- lapply(ecis, function(x) x$pred.seg)
    else Preds <- NULL
  } else {
    Data <- ecis
    Preds <- NULL
  }
  
  ## select by "which"
  if (!is.null(which)) {
    Data <- Data[which]; Preds <- Preds[which]
  } 
  
  ## test color/which vector
  if (!is.null(col) & length(col) != length(Data)) stop("'col' should be a vector with same length as input!")
  
  ## matrix plot type
  if (style == "matrix") {
    DIM <- ceiling(sqrt(length(Data)))
    par(mfrow = c(DIM, DIM)); par(mar = c(2, 2, 1, 1))
    for (i in 1:length(Data)) {
        dat <- Data[[i]]
        plot(dat, pch = 16, cex = 0.5, col = if(!is.null(col)) col[i] else "black", ...)
        if (!is.null(Preds)) lines(dat[, 1], Preds[[i]], col = "darkred", lwd = 2, ...)
        grid()
        if (legend) {
          Well <- strsplit(colnames(dat), "_")[[1]][2]
          legend("topleft", Well, cex = cex.legend)
       }
    }
  }
 
  ## single plot type
  if (style == "single") {
    allX <- lapply(Data, function(x) x[, 1])
    allY <- lapply(Data, function(x) x[, 2])
    rX <- range(allX, na.rm = TRUE); rY <- range(allY, na.rm = TRUE) 
    for (i in 1:length(Data)) {
      dat <- Data[[i]]
      if (i == 1) {
          plot(dat, pch = 16, cex = 0.5, xlim = rX, ylim = rY, 
               col = if(!is.null(col)) col[i] else "black", xlab = "Time(hrs)", ylab = "Imp(ohm)", ...)
          grid()
        }
        else {
          points(dat, pch = 16, cex = 0.5, col = if(!is.null(col)) col[i] else "black", ...)
        }
      if (!is.null(Preds)) lines(dat[, 1], Preds[[i]], col = "darkred", lwd = 2, ...)
    }
  }
}

cutECIS <- function(ecis, min = NULL, max = NULL) {
  if(class(ecis)[1] != "ecis") stop("This is not an 'ecis' object!")
  
  ## cut function
  f <- function(data) {
    MIN <- ifelse(is.null(min), min(data[, 1], na.rm = TRUE), min)
    MAX <- ifelse(is.null(max), max(data[, 1], na.rm = TRUE), max)
    sel <- which(data[, 1] >= MIN & data[, 1] <= MAX)
    return(data[sel, ])
  }
  
  Out <- lapply(ecis, function(x) f(x))
  class(Out) <- c("ecis", "list")
  return(Out)
}

delECIS <- function(ecis, which) {
  if(class(ecis)[1] != "ecis") stop("This is not an 'ecis' object!")
  Out <- ecis[-which]
  class(Out) <- c("ecis", "list")
  return(Out)
}

addECIS <- function(...) {
  Data <- list(...)
  cl <- sapply(Data, function(x) class(x)[1])
  if (!all(cl == "ecis")) stop("All items must be an 'ecis' object!")
  Out <- do.call("c", Data)
  class(Out) <- c("ecis", "list")
  return(Out)
}

baseECIS <- function(ecis, type = c("mean", "median"), window = 2) {
  type <- match.arg(type)
  if(class(ecis)[1] != "ecis") stop("This is not an 'ecis' object!")
  
  ## baselining function
  f <- function(data) {
    sel <- which(data[, 1] < min(data[, 1], na.rm = TRUE) + window)
    if (type == "mean") BASE <- mean(data[sel, 2])
    if (type == "median") BASE <- median(data[sel, 2])
    temp <- data; temp[, 2] <- temp[, 2] - BASE
    return(temp)
  }
  
  Out <- lapply(ecis, function(x) f(x))
  class(Out) <- c("ecis", "list")
  return(Out)
}

normECIS <- function(ecis) {
  if(class(ecis)[1] != "ecis") stop("This is not an 'ecis' object!")
  
  ## normalizing function
  f <- function(data) {
    temp <- data 
    temp[, 2] <- temp[, 2] - min(temp[, 2], na.rm = TRUE)
    temp[, 2] <- temp[, 2] / max(temp[, 2], na.rm = TRUE)
    return(temp)
  }
  
  Out <- lapply(ecis, function(x) f(x))
  class(Out) <- c("ecis", "list", "norm")
  return(Out)
}

## taken from 'sintegral' function of the Bolstad2 package => Simpson's rule
simpson <- function(x, y, min = NULL, max = NULL, n.pts = 256) {
  if (length(x) != length(y)) stop("Unequal input vector lengths")
  if (n.pts < 64) n.pts <- 64; if (length(x) > n.pts) n.pts <- length(x)
  if (!is.null(min)) s1 <- which.min(abs(min - x)) else s1 <- 1
  if (!is.null(max)) s2 <- which.min(abs(max - x)) - 1 else s2 <- length(x)
  x <- x[s1:s2]; y <- y[s1:s2]
  ap <- approx(x, y, n = 2 * n.pts + 1)
  h <- diff(ap$x)[1]
  integral <- h * (ap$y[2 * (1:n.pts) - 1] + 4 * ap$y[2 * (1:n.pts)] + ap$y[2 * (1:n.pts) + 1])/3
  return(list(x = ap$x[2 * (1:n.pts)], y = cumsum(integral), int = sum(integral)))
}

intECIS <- function(ecis, min = NULL, max = NULL) {
  if(class(ecis)[1] != "ecis") stop("This is not an 'ecis' object!")
  Out <- sapply(ecis, function(x) simpson(x[, 1], x[, 2], min = min, max = max)$int)
  names(Out) <- sapply(ecis, function(x) colnames(x)[1])
  return(Out)
}

fit <- function(data, nknots = 200, npsi = 3, norm = FALSE) {
  x <- data[, 1]; y <- data[, 2]
  MIN <- min(y, na.rm = TRUE); MAX <- max(y, na.rm = TRUE)
  P01 <- MIN + 0.1 * (MAX - MIN)
  P02 <- MIN + 0.2 * (MAX - MIN)
  P05 <- MIN + 0.5 * (MAX - MIN)
  
  ####################### SPLINE FIT #######################
  ## spline fit & prediction
  SPL <- smooth.spline(x, y, spar = 1, nknots = nknots)
  pred.spline <- predict(SPL)$y
  
  ## get first and second derivatives, and their maxima
  P1 <- predict(SPL, x, deriv = 1)
  P2 <- predict(SPL, x, deriv = 2)
  FDM.spline <- max(P1$y, na.rm = TRUE); SDM.spline <- max(P2$y, na.rm = TRUE)
  xFDM.spline <- x[which.max(P1$y)]; xSDM.spline <- x[which.max(P2$y)]
  
  ## get Time @ Imp = 0.1
  P01.spline <- try(uniroot(function(x) P01 - predict(SPL, x)$y, lower = 0, upper = max(x, na.rm = TRUE))$root, silent = TRUE)
  if (inherits(P01.spline, "try-error")) {
    print("Error in root finding!")
    P01.spline <- NA
  }
  
  ####################### 4-par LOGISTIC FIT ################
  ## logistic model fit & prediction
  if (norm) {
    NLM <- nlsLM(y ~ 1/(1 + exp(b * (x - e))), 
                 start = list(b = -0.1, e = mean(x, na.rm = TRUE)),
                 control = nls.lm.control(maxiter = 1024))
  } else {
  NLM <- nlsLM(y ~ c + (d-c)/(1 + exp(b * (x - e))), 
               start = list(c = min(y, na.rm = TRUE), 
                            d = max(y, na.rm = TRUE), 
                            b = -5, e = mean(x, na.rm = TRUE)),
               control = nls.lm.control(maxiter = 1024))
  }
  pred.log <- predict(NLM)
  
  ## get first and second derivatives, and their maxima
  if (norm) {
    b <- coef(NLM)[1]; e <- coef(NLM)[2]
    D1 <- -((b * exp(b * (e + x)))/(exp(b * e) + exp(b * x))^2)
    D2 <- (b^2 * exp(b * (e + x)) * (-exp(b * e) + exp(b * x)))/(exp(b * e) + exp(b * x))^3
    FDM.log <- max(D1, na.rm = TRUE); SDM.log <- max(D2, na.rm = TRUE)
    xFDM.log <- x[which.max(D1)]; xSDM.log <- x[which.max(D2)]
  } else {
    b <- coef(NLM)[3]; c <- coef(NLM)[1]; d <- coef(NLM)[2]; e <- coef(NLM)[4]
    D1 <- (b * (c - d) * exp(b * (e + x)))/(exp(b * e) + exp(b * x))^2
    D2 <- (b^2 * (c - d) * exp(b * (e + x)) * (exp(b * e) - exp(b * x)))/(exp(b * e) + exp(b * x))^3
    FDM.log <- max(D1, na.rm = TRUE); SDM.log <- max(D2, na.rm = TRUE)
    xFDM.log <- x[which.max(D1)]; xSDM.log <- x[which.max(D2)]
  }
  
  ## get Time @ Imp = 0.1
  P01.log <- try(uniroot(function(x) P01 - predict(NLM, newdata = data.frame(x = x)), lower = 0, upper = max(x, na.rm = TRUE))$root, silent = TRUE)
  if (inherits(P01.log, "try-error")) {
    print("Error in root finding. Consider using 'normalize' first!")
    P01.log <- NA
  }
  
  ######################## SEGMENTED REGRESSION ##################
  ## segmented regression
  LM <- lm(y ~ x)
  SEG <- segmented(LM, seg.Z = ~x, npsi = npsi, control = seg.control(display = FALSE, n.boot = 20, it.max = 50)) 
  pred.seg <- predict(SEG)
  coef.seg <- coef(SEG); m <- grep("psi", names(coef.seg)); coef.seg <- coef.seg[-m]
  
  ## get 50%-point & slope
  p05.seg <- try(uniroot(function(x) P05 - predict(SEG, newdata = data.frame(x = x)), 
                       lower = SEG$psi[1, 2], upper = max(x, na.rm = TRUE), extendInt = "yes")$root, silent = TRUE)
  if (inherits(p05.seg, "try-error")) {
    print("Error in root finding. Consider using 'normalize' first!")
    p05.seg <- NA; slope05.seg <- NA
  } else {
    sel <- findInterval(p05.seg, SEG$psi[, 2])
    slope05.seg <- coef(SEG)[sel + 2]
  }
  
  ## get 20%-point & slope
  p02.seg <- try(uniroot(function(x) P02 - predict(SEG, newdata = data.frame(x = x)), 
                       lower = SEG$psi[1, 2], upper = max(x, na.rm = TRUE), extendInt = "yes")$root, silent = TRUE)
  if (inherits(p02.seg, "try-error")) {
    print("Error in root finding. Consider using 'normalize' first!")
    p02.seg <- NA; slope02.seg <- NA
  } else {
    sel <- findInterval(p02.seg, SEG$psi[, 2])
    slope02.seg <- coef(SEG)[sel + 2]
  }
  
  ## get 10%-point & slope
  p01.seg <- try(uniroot(function(x) P01 - predict(SEG, newdata = data.frame(x = x)), 
                 lower = SEG$psi[1, 2], upper = max(x, na.rm = TRUE), extendInt = "yes")$root, silent = TRUE)
  if (inherits(p01.seg, "try-error")) {
    print("Error in root finding. Consider using 'normalize' first!")
    p01.seg <- NA; slope01.seg <- NA
  } else {
    sel <- findInterval(p01.seg, SEG$psi[, 2])
    slope01.seg <- coef(SEG)[sel + 2]
  }
  
  OUT <- list(fdm.spline = as.numeric(FDM.spline), sdm.spline = as.numeric(SDM.spline), xfdm.spline = as.numeric(xFDM.spline), 
              xsdm.spline = as.numeric(xSDM.spline), x01.spline = as.numeric(P01.spline), pred.spline = as.numeric(pred.spline), 
              model.spl = SPL, fdm.log = as.numeric(FDM.log), sdm.log = as.numeric(SDM.log), xfdm.log = as.numeric(xFDM.log),
              xsdm.log = as.numeric(xSDM.log), x01.log = as.numeric(P01.log), coef.log = coef(NLM), 
              pred.log = as.numeric(pred.log), model.log = NLM, p05.seg = as.numeric(p05.seg), slope05.seg = as.numeric(slope05.seg), 
              p02.seg = as.numeric(p02.seg), slope02.seg = as.numeric(slope02.seg), p01.seg = as.numeric(p01.seg), 
              slope01.seg = as.numeric(slope01.seg), coef.seg = coef.seg, psi.seg = SEG$psi[, 2], 
              pred.seg = as.numeric(pred.seg), model.seg = SEG, data = data)
}

fitECIS <- function(ecis, which = NULL, nknots = 200, npsi = 3) {
  if(class(ecis)[1] != "ecis") stop("This is not an 'ecis' object!")
  if(length(class(ecis)) == 3 & class(ecis)[3] == "norm") norm <- TRUE else norm <- FALSE
  
  ## data selection
  if (!is.null(which)) Data <- ecis[which] else Data <- ecis
  Out <- vector("list", length(Data))
  Names <- vector("character", length(Data))
  
  ## run 'fit' for each curve
  for (i in 1:length(Data)) {
    dat <- Data[[i]]
    Well <- strsplit(colnames(dat)[1], "_")[[1]][2]
    cat("Analyzing sample #", i, " => Well ", Well, "\n", sep = "")
    Out[[i]] <- fit(dat, nknots = nknots, npsi = npsi, norm = norm)
    Names[i] <- Well
  }
  
  class(Out) <- c("ecis", "fit", "list")
  attr(Out, "names") <- Names
  return(Out)
}

parECIS <- function(fit) {
  if (class(fit)[2] != "fit") stop("This is not an ECIS fit list!")
  OUT <- sapply(fit, function(x) unlist(x[-c(6, 7, 14, 15, 24:26)]))
  colnames(OUT) <- attr(fit, "names")
  return(OUT)
}  

extend <- function(dat, extend = NULL, n = 10) {
  x <- dat[, 1]; y <- dat[, 2]; cn <- colnames(dat)
  START <- min(x, na.rm = TRUE); END <- max(x, na.rm = TRUE)
  if (extend >= START & extend <= END) stop("'extend' must not be in data range => ", START, " / ", END)
  DIFF <- mean(diff(x)[1:100], na.rm = TRUE)
  
  if (extend < START) {
    Med <- median(y[1:n], na.rm = TRUE)
    xSeq <- seq(extend, START - DIFF, by = DIFF)
    ySeq <- rep(Med, length(xSeq))
  }
  
  if (extend > END) {
    LEN <- length(x)
    Med <- median(y[(LEN - n + 1):LEN], na.rm = TRUE)
    xSeq <- seq(END + DIFF, extend, by = DIFF)
    ySeq <- rep(Med, length(xSeq))
  }
  
  OUT <- cbind(c(xSeq, x), c(ySeq, y))
  colnames(OUT) <- cn
  return(OUT)
}

extECIS <- function(ecis, extend = NULL, n = 10) {
  if(class(ecis)[1] != "ecis") stop("This is not an 'ecis' object!")
  Out <- lapply(ecis, function(x) extend(x, extend = extend, n = n))
  class(Out) <- c("ecis", "list")
  return(Out)
}

anomaly <- function(dat, spar, fac) {
  x <- dat[, 1]; y <- dat[, 2]; cn <- colnames(dat)
  SPL <- smooth.spline(x, y, spar = spar)
  resid <- SPL$y - y
  iqr <- IQR(resid)
  sel <- which(abs(y) > SPL$y + fac * iqr)
  y[sel] <- SPL$y[sel]
  Out <- cbind(x, y); colnames(Out) <- cn
  return(list(dat = Out, sel = sel))
}

anoECIS <- function(ecis, spar = 1, fac = 10) {
  if(class(ecis)[1] != "ecis") stop("This is not an 'ecis' object!")
  Out <- lapply(ecis, function(x) anomaly(x, spar = spar, fac = fac)$dat)
  class(Out) <- c("ecis", "list")
  return(Out)
}

set <- function(dat, time = NULL, trim = FALSE) {
  if (is.null(time)) stop("'time' must be a numeric value")
  if (time < min(dat[, 1], na.rm = TRUE) | time > max(dat[, 1], na.rm = TRUE)) stop("'time' must be in time range") 
  dat[, 1] <- dat[, 1] - time
  if (trim) dat <- dat[dat[, 1] >= 0, ]
  return(dat)
}

setECIS <- function(ecis, time = NULL, trim = FALSE) {
  if(class(ecis)[1] != "ecis") stop("This is not an 'ecis' object!")
  Out <- lapply(ecis, function(x) set(x, time = time, trim = trim))
  class(Out) <- c("ecis", "list")
  return(Out)
}
