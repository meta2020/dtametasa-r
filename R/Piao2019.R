##------------------------------------------------------------------------------
##
## HECKMANTYPE SELECTION FUNCTION IN Piao et al. (2019)
##
##------------------------------------------------------------------------------

## EM-ALGORITHM

##
## E-STEP ---- 
##

.E.m <- function(
  par, y1, y2, v1, v2
  ){ 
  
  beta1 <- par[1]; beta2 <- par[2]  ## mu1, mu2
  tau1  <- par[3]; tau2 <- par[4]; rho.b <- par[5]  ##tau1, tau2, rho
  
  rho1  <- par[6]; rho2 <- par[7]
  gamma0 <- par[8]; gamma1 <- par[9]; gamma2 <- par[10]
  
  
  
  s1 <- sqrt(v1); s2 <- sqrt(v2)
  
  temp1 <- gamma0 + gamma1 / s1 + gamma2 /s2
  
  temp2.denom <- vapply(1:length(y1), function(i){
    
    Sigma.i12 <- c(rho1 * s1[i], rho2 * s2[i])
    Sigma.i22 <- matrix(c(tau1^2 + v1[i], rho.b * tau1 * tau2, rho.b * tau1 * tau2, tau2^2 + v2[i]), 2, 2)
    D.i       <- c(y1[i] - beta1, y2[i] - beta2) 
    
    temp2 <- Sigma.i12 %*% solve(Sigma.i22) %*% D.i
    
    sigma.c <- 1 - Sigma.i12 %*% solve(Sigma.i22) %*% Sigma.i12
    
    c(temp2, sigma.c)
    
  }, c(temp2 = 0, denom = 0))
  
  temp2 <- temp2.denom[1,]
  denom <- sqrt(temp2.denom[2,])
  
  # calculating P[Z>0|theta]  <-  \Phi[v_i]
  
  p1 <- pnorm((temp1+temp2)/denom)
  p2 <- pnorm(-(temp1+temp2)/denom)
  
  # calculating E[m] <- (1-p)/p (expected # of unpublished studies)
  
  return(p2/p1)  
}

##
## M-STEP ----
##

.E.loglik <- function(
  par, y1, y2, v1, v2, m
  ){ 
  
  beta1 <- par[1]; beta2 <- par[2]  ##mu1, mu2
  tau1  <- par[3]; tau2  <- par[4]; rho.b <- par[5]  ## tau1, tau2, rho
  
  rho1   <- par[6]; rho2   <- par[7]
  gamma0 <- par[8]; gamma1 <- par[9]; gamma2 <- par[10]
  

  
  s1 <- sqrt(v1); s2 <- sqrt(v2)
  
  temp1 <- gamma0 + gamma1 / s1 + gamma2 /s2
  
  vec.res <- vapply(1:length(y1), function(i){
    
    Sigma.i12 <- c(rho1 * s1[i], rho2 * s2[i])
    Sigma.i22 <- matrix(c(tau1^2 + v1[i], rho.b * tau1 * tau2, rho.b * tau1 * tau2, tau2^2 + v2[i]), 2, 2)
    D.i       <- c(y1[i] - beta1, y2[i] - beta2) 
    
    inv.Sigma.i22 <- solve(Sigma.i22)
    
    temp2   <- Sigma.i12 %*% inv.Sigma.i22 %*% D.i
    
    sigma.c <- 1 - Sigma.i12 %*% inv.Sigma.i22 %*% Sigma.i12
    
    log.det <- log(det(Sigma.i22))
    
    DSD     <- t(D.i) %*% inv.Sigma.i22 %*% D.i
    
    c(temp2, sigma.c, log.det, DSD)
    
  }, c(temp2 = 0, denom = 0, log.det = 0, DSD = 0))
  
  temp2 <- vec.res[1,]
  
  junk1 <- temp1 + temp2
  junk2 <- sqrt(vec.res[2,])
  
  v <- junk1/junk2
  
  ###bounding V?
  v[v < -37] <- -37
  v[v > 37]  <- 37
  p1 <- pnorm(v)
  p2 <- pnorm(-v)
  
  ###full log likelihood 
  
  log.det <- vec.res[3, ]; DSD <- vec.res[4, ]
  
  result <- log(p1) + m*log(p2) - (m+1)/2*log.det - (m+1)/2*DSD
  
  return(-sum(result)) 
  
  ###often prefer to minimize neg. log likelihood instead of max
  
}


## HECKMAN-TYPE ----

Piao2019 <- function(
  data, init, tol=1e-10, maxit=1000
  ){
  
  
  n <- nrow(data)
  
  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2

  par.new <- init # Initialize values
  counter <- 0    # start counter
  
  while (counter <= maxit) {
  
    counter <- counter + 1
    par.old <- par.new
    
    ## E-step for EM algorithm
    myM <- .E.m(par.old, y1, y2, v1, v2)  

    fn <- function(par) .E.loglik(par, y1, y2, v1, v2, myM)
    
    ## M-step for EM algorithm
    ## par: beta1, beta2, tau1, tau2, rho.b, rho1, rho2, gamma0, gamma1, gamma2
    
    eps <- sqrt(.Machine$double.eps)
    
    output <- nlminb(
      par.old, fn, 
      lower=c(-5, -5, eps, eps, -1, -1, -1, -2, 0, 0), 
      upper=c( 5,  5,   2,   2,  1,  1,  1,  2, 2, 2), 
      control = list(maxit = 1000), hessian=FALSE
      )
    
    par.new <- output$par
    
    if (max(abs(par.new-par.old)) < tol){  ###if convergence achieved
      
      return(list(par = par.new, conv=TRUE))
    
    }
  }
  

  return(list(
    par = par.new,
    conv=FALSE)) 
  
}  
