##******************************************************************************
##
## PLOT SROC OR SROC MATRIX
##
##******************************************************************************

# 
# SROC <- function(object,
#                  add = FALSE,
#                  sroc.col = 1,
#                  sroc.lty = 1,
#                  sroc.lwd = 1,
#                  add.spoint = TRUE,
#                  spoint.pch = 18,
#                  spoint.col = 1,
#                  spoint.cex = 2,
#                  xlab = "FPR",
#                  ylab = "TPR",
#                  ...) {
# 
#   if(inherits(object,"dtametasa")) par.vec <- object$par[c(1,2,4,5)] else {
# 
#     if (is.vector(object) & length(object) >= 4) {
# 
#       par.vec <- object} else stop("PLEASE INPUT EITHER dtametasa OBJECTS OR A VECTOR OF c(u1, u2, t22, t12)")
# 
#   }
# 
#   u1  <- par.vec[1]
#   u2  <- par.vec[2]
#   t22 <- par.vec[3]
#   t12 <- par.vec[4]
# 
#   roc   <- function(x) plogis(u1 - (t12/t22) * (qlogis(x) + u2))
# 
#   curve(roc, xlab = xlab, ylab = ylab, add = add, col = sroc.col, lwd =sroc.lwd,lty = sroc.lty,
#         xlim = c(0,1), ylim = c(0,1), ...)
# 
#   if(add.spoint) points(plogis(-u2), plogis(u1), pch = spoint.pch, col = spoint.col, cex = spoint.cex, ...)
# 
# 
# }
# 



SROC.matrix <- function(par,  ## u1 u2 t12 t22
                 add = FALSE,
                 ncols = NULL,
                 sroc.lty = 1,
                 sroc.lwd = 1,
                 add.spoint=TRUE,
                 legend = FALSE,
                 p.vec,
                 legend.text = paste0("p = ",p.vec),
                 legend.cex = 1,
                 spoint.pch = 18,
                 spoint.cex = 2,
                 xlab = "FPR",
                 ylab = "TPR") {

  # if(length(par) < 4) stop("PLEASE CHECK THE INPUT VECTOR")

  # if(length(par) == 4) 
    par <- as.matrix(par)

  # if(nrow(par) < 4) stop("PLEASE CHECK THE INPUT MATRIX")

  if (!add) plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab = xlab, ylab = ylab)

  if (is.null(ncols)) ncols <- gray.colors(ncol(par), gamma = 1, start = 0, end = 0.8)

  for (i in 1:ncol(par)) {

    u1  <- par[1,i]
    u2  <- par[2,i]
    t22 <- par[3,i]
    t12 <- par[4,i]

    roc <- function(x) plogis(u1 - (t12/t22) * (qlogis(x) + u2))
    curve(roc, 0, 1, col = ncols[i], add = TRUE,
          lty = sroc.lty, lwd = sroc.lwd, ...)
  }

  if (legend) legend("bottomright",
                    legend = legend.text,
                    col = ncols,
                    lty = rep(sroc.lty, ncol(par)),
                    cex = legend.cex,
                    ...)

  if (add.spoint) {
    sens <- plogis(par[1,])
    spec <- plogis(par[2,])
    points(1-spec, sens, col=ncols, pch = spoint.pch, cex = spoint.cex, ...)
  }

}

