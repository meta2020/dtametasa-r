##******************************************************************************
##
## TRUE PARAMETERS IN THE SIMULATION DATA
##
##******************************************************************************

source("../../functions/sauc.R")

##******************************************************************************
##
## A TOTAL NUMBER OF 6 SCENARIOS, IN 3 SAMPLE SIZES
##
## P=0.7, b=0.5, t1=t2=sqrt(0.5) ----
##
## "set-t0.7-c11.RData": c1=c2 
## "set-t0.7-c10.RData": c1=1, c2=0
## "set-t0.7-c01.RData": c1=0, c2=1
##
##******************************************************************************


set <- lapply(c(15, 25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(0.5, 6) 
  t22 <- rep(0.5, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c(-0.4229741, -0.4606838, -1.0031287, -1.0315272, -0.4566476, -0.4917864)
  
  c11 <- rep(0.5, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, sqrt(t11), sqrt(t22), r))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t0.7-c11.RData")


set <- lapply(c(15, 25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(0.5, 6) 
  t22 <- rep(0.5, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c(0.7935728,  0.7946827, -0.6978995, -0.7009921, -1.3623635, -1.3616461)
  
  c11 <- rep(1, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, sqrt(t11), sqrt(t22), r))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t0.7-c10.RData")


set <- lapply(c(15, 25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(0.5, 6) 
  t22 <- rep(0.5, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c(-0.9931601, -0.9960329, -0.6966569, -0.6975191,  1.3419139,  1.3351313)
  
  c11 <- rep(0, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, sqrt(t11), sqrt(t22), r))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t0.7-c01.RData")

