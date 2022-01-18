##******************************************************************************
##
## CREATE POPULATIN DATA
##
##******************************************************************************

# par <- c(20, 0, 1.73, 0.5, 0.5, -0.15, 0.5, 0.5); DT  <- sim.pdata(par)

.sim.pdata <- function(par){


  # S <- 20; u1 <- 0; u2 <- 1.73; t11 <- 0.5; t22 <- 0.5; t12 <- -0.15; c11 <- 0; c22 <- 1; b <- 0.5; a <- -0.99; suac <- 0.62; r <- -0.3; se <- 0.5; sp <- 0.85
  S  <- par[1]
  u1 <- par[2]
  u2 <- par[3]

  t11 <- par[4]
  t22 <- par[5]
  t12 <- par[6]

  c11 <- par[7]
  c22 <- par[8]

  c1 <- sqrt(c11)
  c2 <- sqrt(c22)

  v1 <- rnorm(S, 0.5, 0.5)^2
  v2 <- rnorm(S, 0.5, 0.5)^2

  u   <- c(u1, u2)

  ## Sigma+Omega

  SO <- lapply(1:S, function(s){

    matrix(c(v1[s]+t11, t12, t12, v2[s]+t22),2,2)

  })

  ## CHECK PD

  check.PD <- sapply(1:S, function(s){

    eigen(SO[[s]])$values

  })

  if (any(as.vector(check.PD)<= 0)) stop("VAR-COV MATRIX (S+O) is NOT PD")

  ## y FROM N(u, SO)

  Y <- t(sapply(1:S, function(s) mvtnorm::rmvnorm(1, u, SO[[s]])))

  ## SENS AND SPEC

  X <- plogis(Y)

  ## FINAL DATAFRMAE WITH NAME (y1, y2, v1, v2)

  ## AUGMENTED T-LNDOR
  ldor.t <- (c1*Y[,1]+c2*Y[,2])/sqrt(c11*v1+c22*v2)

  DT <- cbind.data.frame(Y, v1, v2, ldor.t, X)

  colnames(DT) <- c("y1", "y2", "v1", "v2", "t.clnDOR", "se", "sp")

  DT
}
