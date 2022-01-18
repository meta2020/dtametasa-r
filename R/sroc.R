##******************************************************************************
##
## PLOT SROC
##
##******************************************************************************


SROC <- function(
  par,  ## mu1 mu2 tau1 tau2 rho
  sroc.type = c("sroc", "hsroc"),
  add = FALSE,
  ncols = NULL,
  add.spoint=TRUE,
  spoint.pch = 18,
  spoint.cex = 2,
  xlab = "FPR",
  ylab = "TPR",
  ...
){
    
    par <- as.matrix(par)
    
    if (!add) plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab = xlab, ylab = ylab, ...)
    
    if (is.null(ncols)) ncols <- gray.colors(ncol(par), gamma = 1, start = 0, end = 0.8)
    
    for (i in 1:ncol(par)) {
      
      u1  <- par[1,i]
      u2  <- par[2,i]
      t1  <- par[3,i]
      t2  <- par[4,i]
      
      if(sroc.type=="sroc") r <- par[5, i] else r <- -1
      
      roc <- function(x) plogis(u1 - (t1*r/t2) * (qlogis(x) + u2))
      curve(roc, 0, 1, col = ncols[i], add = TRUE,
            lty = 1, lwd = 1)
    }
    
    
    if (add.spoint) {
      sens <- plogis(par[1,])
      spec <- plogis(par[2,])
      points(1-spec, sens, col=ncols, pch = spoint.pch, cex = spoint.cex)
    }
    
  }

