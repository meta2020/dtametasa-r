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
## P=0.7, b=0.5, t1=1, t2=2 ----
##
## "set-t12-c11.RData": c1=c2 
## "set-t12-c10.RData": c1=1, c2=0
## "set-t12-c01.RData": c1=0, c2=1
##
##******************************************************************************


set <- lapply(c(15, 25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(1, 6) 
  t22 <- rep(4, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c(-0.1650742, -0.2509737, -0.7662483, -0.8483632, -0.1979666, -0.2837208)
  
  c11 <- rep(0.5, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, sqrt(t11), sqrt(t22), r))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t12-c11.RData")


set <- lapply(c(15, 25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(1, 6) 
  t22 <- rep(4, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c(0.8913684,  0.8939144, -0.5701478, -0.5729787, -1.2693381, -1.2691164)
  
  c11 <- rep(1, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, sqrt(t11), sqrt(t22), r))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t12-c10.RData")


set <- lapply(c(15, 25, 50, 200), function(S) {
  
  S   <- rep(S, 6)
  se  <- c(rep(0.5, 2), rep(0.8, 2), rep(0.9, 2))
  sp  <- c(rep(0.85,2), rep(0.8, 2), rep(0.4, 2))
  u1  <- qlogis(se)
  u2  <- qlogis(sp)
  
  t11 <- rep(1, 6) 
  t22 <- rep(4, 6) 
  r   <- rep(c(-0.3, -0.6), 3)
  t12 <- sqrt(t11*t22)*r
  
  b   <- rep(0.5, 6)
  a   <- c(-0.4290629, -0.4333711, -0.1106175, -0.1181312,  1.7440464,  1.7326804)
  
  c11 <- rep(0, 6)
  c22 <- 1-c11
  
  sauc<- SAUC(rbind(u1, u2, sqrt(t11), sqrt(t22), r))
  
  cbind(S, u1, u2, t11, t22, t12, c11, c22, b, a, sauc, r, se, sp)
  
})

save(set, file = "set-t12-c01.RData")

