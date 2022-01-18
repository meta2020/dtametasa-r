##******************************************************************************
##
## PROPOSED MODEL, NOT SPECIFY C VECTOR, SA2 (FULL VERSION)
## USED IN THE EXAMPLE
##
##******************************************************************************



dtametasa.rc <- function(
  data,
  p = 0.8,
  beta.init = 1,
  c1.sq.init = 0.5,
  beta.interval = c(0, 2),
  alpha.interval = c(-3, 3),
  ci.level = 0.95,
  show.warn.message = FALSE,
  alpha.root.extendInt = "downX",
  sauc.type = c("sroc", "hsroc")
){
    
    ##
    ## INPUT: DATA PREPROCESS ----------------------------------------------------------
    ##
    
    n <- nrow(data)
    
    if ("TP" %in% names(data)){
      
      data <- correction(data, value = 0.5, type="single")
      
      data <- logit.data(data)
      
    }
    
    y1 <- data$y1
    y2 <- data$y2
    v1 <- data$v1
    v2 <- data$v2
    
    
    ##
    ##  INPUT: OPTIMIZATION LOGLIKELIHOOD FUNCTION --------------------------------------
    ##
    
    
    ## AUTO-SET START POINTS
    
    sa.rc.init <- c(mu1 = 0, mu2 = 0, tau1 = 0.1, tau2 = 0.1, rho = -0.1, beta = beta.init, c1 = sqrt(0.5))
    
    fit.m <- mvmeta::mvmeta(cbind(y1,y2),S=cbind(v1, rep(0, n), v2), method="ml")
    
    if(!inherits(fit.m, "try-error")) {
      
      if(fit.m$converged){
        
        p1 <- sqrt(fit.m$Psi[1])
        p2 <- sqrt(fit.m$Psi[4])
        p.r<- fit.m$Psi[3]/(p1*p2)
        
        sa.rc.init <- c(mu = unname(fit.m$coefficients), tau1 = p1, tau2 = p2, rho = p.r, beta = beta.init, c1 = sqrt(c1.sq.init))
        
      } 
      
    }
    
    eps <- .Machine$double.eps^0.5
    
    fn <- function(par) .llk.o(
      par,
      y1, y2, v1, v2, n, p,
      alpha.interval,
      alpha.root.extendInt,
      show.warn.message)
    
    opt <- try(nlminb(
      sa.rc.init,
      fn,
      lower = c(-5, -5, eps, eps,-1, beta.interval[1], 0),
      upper = c( 5,  5, 3, 3,  1, beta.interval[2], 1)
    ), silent = TRUE)
    
    if(!inherits(opt,"try-error")) {
      
      
      ##
      ##  OUTPUT: ALL PARAMETERS -------------------------------------------------
      ##
      
      u1  <- opt$par[1]; se  <- plogis(u1)
      u2  <- opt$par[2]; sp  <- plogis(u2)
      
      t1  <- opt$par[3]; t11 <- t1^2
      t2  <- opt$par[4]; t22 <- t2^2
      r   <- opt$par[5]; t12 <- t1*t2*r
      
      b   <- opt$par[6]
      
      c1  <- opt$par[7]; c11 <- c1^2
      c22 <- 1-c11; c2  <- sqrt(c22)
      
      
      u.ldor   <- c1*u1 + c2*u2
      t.ldor   <- c11*t11 + c22*t22 + 2*c1*c2*t12
      
      se.ldor2 <- c11*v1+c22*v2
      se.ldor  <- sqrt(se.ldor2)
      
      sq     <- sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))
      
      ##
      ##  HESSIANS -------------------------------------------------
      ##
      
      hes <- numDeriv::hessian(fn, opt$par)
      rownames(hes) <- colnames(hes) <- c("mu1", "mu2", "tau1", "tau2", "rho", "beta", "c1")
      
      ##
      ## SAUC CI -------------------------------------------------
      ##
      
      if(p==1) inv.I.fun.m <- solve(hes[1:5,1:5]) else inv.I.fun.m <- solve(hes)
      
      opt$var.ml <- inv.I.fun.m
        
      var.matrix <-  inv.I.fun.m[1:5, 1:5]
      
      opt$sauc.ci <- SAUC.ci(u1 = u1, u2 = u2, t1 = t1, t2 = t2, r = r, var.matrix = var.matrix, sauc.type = sauc.type, ci.level = ci.level)
      
      
      #
      # b CI -------------------------------------------------
      #
      if(p==1) opt$beta.ci <- c(NA, NA, NA) else {
        
        b.se <- suppressWarnings(sqrt(solve(hes)[6,6]))
        b.lb <- b + qnorm((1-ci.level)/2, lower.tail = TRUE)*b.se
        b.ub <- b + qnorm((1-ci.level)/2, lower.tail = FALSE)*b.se
        
        opt$beta.ci <- c(b, b.lb, b.ub)
        
      }
      
      names(opt$beta.ci) <- c("beta", "beta.lb", "beta.ub")
      
      ##
      ## ALPHA CALC --------------------------------------------------------
      ##
      
      if(p==1) a.opt <- NA else {
        
        .a.p <- function(a) { sum(1/ pnorm( (a + b * u.ldor/se.ldor) / sq ), na.rm = TRUE) - n/p }
        
        if (!show.warn.message) a.opt.try <- suppressWarnings(try(uniroot(.a.p, interval = alpha.interval, extendInt = alpha.root.extendInt), silent = TRUE)) else a.opt.try <- try(uniroot(.a.p, interval = alpha.interval, extendInt=alpha.root.extendInt), silent = TRUE)
        
        a.opt <- a.opt.try$root
        
      }
      
      opt$alpha <- c(alpha = a.opt)
      
      ##
      ## mu1 CI -------------------------------------------------
      ##
      
      u1.se <- suppressWarnings(sqrt(inv.I.fun.m[1,1]))
      u1.lb <- u1 + qnorm((1-ci.level)/2, lower.tail = TRUE)*u1.se
      u1.ub <- u1 + qnorm((1-ci.level)/2, lower.tail = FALSE)*u1.se
      
      opt$mu1.ci <- c(u1, u1.lb, u1.ub, se, plogis(u1.lb), plogis(u1.ub))
      names(opt$mu1.ci) <- c("mu1", "mu1.lb", "mu1.ub", "sens", "se.lb", "se.ub")
      
      ##
      ## mu2 CI -------------------------------------------------
      ##
      
      u2.se <- suppressWarnings(sqrt(inv.I.fun.m[2,2]))
      u2.lb <- u2 + qnorm((1-ci.level)/2, lower.tail = TRUE)*u2.se
      u2.ub <- u2 + qnorm((1-ci.level)/2, lower.tail = FALSE)*u2.se
      
      opt$mu2.ci <- c(u2, u2.lb, u2.ub, sp, plogis(u2.lb), plogis(u2.ub))
      names(opt$mu2.ci) <- c("mu2", "mu2.lb", "mu2.ub", "spec", "sp.lb", "sp.ub")
      
      
    }
    
    opt
    
  }
