##******************************************************************************
## 
## SIMULATION EXPERIMENT
##
##
## OUTCOMES: SA2:         THE PROPOSED MODEL (ESTIMATE C1 C2)
##           SA1.CORRECT: THE PROPOSED MODEL (GIVEN C1=TRUE VALUE) 
##           SA1.WRONG:   THE PROPOSED MODEL (GIVEN MISSPECIFIED C1) 
##           HECKMAN:     HECKMAN-TYPE SELECTION MODEL (PIAO ET AL. 2019)
##           REISTMA_O:   REISTMA MODEL BASED ON THE OBSERVED DATA
##           REITSMA_P:   REISTMA MODEL BASED ON THE POPULATION DATA
## 
##******************************************************************************


##
## ONE-TIME ---------------------------------------------------------
##

simu <- function(list.n, 
                 row.n, 
                 beta0, 
                 csq0,  # initial
                 #csq.c, # correct
                 csq.w, # wrong 
                 beta.interval, 
                 alpha.interval){ 
  
  # rm(list = ls())
  # .files.sources <- list.files("../R/"); sapply(paste0("../R/", .files.sources), source)
  # load("../simulation-results/scenarios/scenarios-t12/set-t12-c11.RData")
  # list.n <- row.n <- 1; beta0 <- 0.5; beta.interval <- c(0,2); alpha.interval <- c(-2,2); csq.w <-1; csq0 <- 0.5; library(mixmeta)

  conf <- set[[list.n]][row.n,]
  
  S  <- conf[1]
  b  <- conf[9]
  a  <- conf[10]
  true.c11<- conf[7]
  
  ## DERIVE DATA (FULL SET: BN)
  
  ##
  ## PDATA -----------------------------------------------------------
  ##
  
  pdata <- .sim.pdata(conf[c(1:8)])
  
  
  ml.p <- ml.s <- ml.sa1.c <- ml.sa1.w <- ml.sa2<- ml.hec <- rep(NA, 9)
  
  
  ##
  ## E.P --------------------------------------------------------
  ##
  
  t <- pdata$t.clnDOR
  p.a <- pnorm(a + b*t)
  p.e <- mean(p.a, na.rm = TRUE)
  
  
  ##
  ## SDATA ------------------------------------------------------------
  ##
  
  s.id <- sapply(1:S, function(i) rbinom(1, 1, p.a[i])) == 1
  p.p  <- sum(s.id)/S
  
  ##  1. ml.P FOR PDATA ------------------------------------------------------------
  
  fit.p <- try(mixmeta(cbind(y1,y2),S=cbind(v1, rep(0, S), v2), data=pdata, method = "ml"), silent = TRUE)
  
  if((!inherits(fit.p, "try-error")) ) {
    
    if(fit.p$converged){
      
      v <- fit.p$Psi
      
      ml.p[c(1:5, 9)]<- c(fit.p$coefficients, sqrt(v[c(1,4)]), v[3]/prod(sqrt(v[c(1,4)])), fit.p$converged-1)
      
    } 
    
  } 
  
  ##  2. ml.S FOR SDATA (PB EXISTS) -------------------------------------------------
  
  if(TRUE %in% s.id) {
    
    sdata<- pdata[s.id,] 
    
    fit.s <- try(mixmeta(cbind(y1,y2),S=cbind(v1, rep(0, nrow(sdata)), v2), 
                        data=sdata, method = "ml"), silent=TRUE)
    
    if(!inherits(fit.s, "try-error")) {
      
      if(fit.s$converged){
        
        v <- fit.s$Psi
        
        ml.s[c(1:5, 9)]<- c(fit.s$coefficients, sqrt(v[c(1,4)]), v[3]/prod(sqrt(v[c(1,4)])), fit.s$converged-1)
        
      } 
      
    } 
    
  }
  
  if (!(NA %in% ml.s[1:5])) start0 <- ml.s[1:5] else start0 <- c(0, 0, 0.1, 0.1, -0.1)   ##


  ## 3. ml.sa1 TRUE FOR SDATA (ADJUST PB)  -------------------------------------------
  
  sa1.c <- try(dtametasa.fc.lit(
    data = sdata, 
    p = p.e,
    c1.sq = true.c11,  
    brem.init = start0,
    beta.init = beta0, 
    beta.interval = beta.interval,
    alpha.interval = alpha.interval,
    show.warn.message = FALSE
  ), silent = TRUE)
  
  if(!inherits(sa1.c, "try-error")) if(sa1.c$convergence == 0) ml.sa1.c <- c(sa1.c$par, sa1.c$convergence)  

  
  ## 4. ml.sa1 WRONG FOR SDATA (ADJUST PB)   -------------------------------------------
  
  sa1.w <- try(dtametasa.fc.lit(
    data = sdata, 
    p = p.e,
    c1.sq = csq.w,  
    brem.init = start0,
    beta.init = beta0, 
    beta.interval = beta.interval,
    alpha.interval = alpha.interval,
    show.warn.message = FALSE
  ), silent = TRUE)
  
  if(!inherits(sa1.w, "try-error")) if(sa1.w$convergence == 0) ml.sa1.w <- c(sa1.w$par, sa1.w$convergence)  
  
  
  ## 5. ml.sa2 FOR SDATA (ADJUST PB) ------------------------------------------------
  
  sa2 <- try(dtametasa.rc.lit(
    data = sdata, 
    p = p.e,   
    brem.init = start0,
    c1.sq.init = csq0,
    beta.init = beta0,
    beta.interval = beta.interval,
    alpha.interval = alpha.interval,
    show.warn.message = FALSE
  ), silent = TRUE)
  
  if(!inherits(sa2, "try-error")) if(sa2$convergence == 0) ml.sa2 <- c(sa2$par, sa2$convergence)
  
  
  ## 6. ml.hec FOR SDATA (Piao 2019) ------------------------------------------------
  
  start.hec <- c(0, 0, 0.1, 0.1, -0.1, 0.2, 0.5, -0.9, 0.3, 0.3)
  
  if (!(NA %in% ml.s[1:5])) start.hec[1:5] <- ml.s[1:5] 
  
  
  hec <- try(Piao2019(data = sdata, init = start.hec), silent = TRUE)
  
  if(!inherits(hec, "try-error")) if(hec$conv == TRUE) ml.hec[c(1:5, 9)] <- c(hec$par[1:5], hec$conv-1)
    
  ## COMBINE RES ------------------------------------------------
  
  
  x   <- cbind(ml.sa2, ml.sa1.c, ml.sa1.w, ml.hec, ml.s, ml.p)
  
  auc <- SAUC(x[c(1:5),])
  p.hat <- c(rep(NA, 4), p.e, p.p)
  
  res <- rbind(x, auc, p.hat)
  rownames(res) <- c("u1", "u2", "t1", "t2", "r", "b", "a", "c1" ,"conv", "SAUC", "p.e")
  colnames(res) <- c("SA2", "SA1.C", "SA1.W", "HEC.O", "BRE.O", "BRE.P")
  
  res
  
}  

