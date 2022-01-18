##******************************************************************************
##
## PROPOSED MODEL, SPECIFY C VECTOR -- FIX C 
## SIMPLE VERSION FOR THE SIMULATION
##
##******************************************************************************


dtametasa.fc.lit <- function(
  data,   
  p,
  c1.sq, 
  brem.init,  
  beta.init,
  beta.interval,
  alpha.interval,
  show.warn.message
){

  ##
  ## INPUT: DATA PREPROCESS  ----------------------------------------------------------
  ##

  n <- nrow(data)

  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2


  c11 <- c1.sq
  c22 <- 1-c11
  c1  <- sqrt(c11)
  c2  <- sqrt(c22)

  ##
  ##  INPUT: OPTIMIZATION LOGLIKELIHOOD FUNCTION --------------------------------------
  ##
    
  start6 <- c(brem.init, beta.init) 


  eps <- sqrt(.Machine$double.eps)
  
  fn <- function(par) .llk.o(
    par = c(par[1:6], c1),
    y1, y2, v1, v2,
    n, p,
    alpha.interval,
    alpha.root.extendInt = "downX",
    show.warn.message = FALSE
  )
                            

  opt <- try(nlminb(start6,
                   fn,
                   lower = c(-5, -5, eps, eps, -1, beta.interval[1]),
                   upper = c( 5,  5, 3, 3,      1 ,beta.interval[2])
  ), silent = TRUE)


  if(!inherits(opt,"try-error")) {


   
    ##
    ##  OUTPUT: ALL PARAMETERS -------------------------------------------------
    ##

    u1  <- opt$par[1]
    u2  <- opt$par[2]
    t1  <- opt$par[3]
    t11 <- t1^2
    t2  <- opt$par[4]
    t22 <- t2^2
    r   <- opt$par[5]
    t12 <- t1*t2*r

    b  <- opt$par[6]

    t.ldor <- c11*t11 + c22*t22 + 2*c1*c2*t12
    u.ldor <- c1*u1 + c2*u2

    se.ldor2 <- c11*v1+c22*v2
    se.ldor  <- sqrt(se.ldor2)

    sq     <- sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))

    ##
    ## ALPHA CALC --------------------------------------------------------
    ##

   .a.p <- function(a) { sum(1/ pnorm( (a + b * u.ldor/se.ldor) / sq ), na.rm = TRUE) - n/p }

    if (!show.warn.message) a.opt.try <- suppressWarnings(try(uniroot(.a.p, interval = alpha.interval, extendInt = "downX"), silent = TRUE)) else a.opt.try <- try(uniroot(.a.p, interval = alpha.interval, extendInt="downX"), silent = FALSE)

    a.opt <- a.opt.try$root
    
    ##
    ## PAR --------------------------------------------------------
    ##
    
    # opt$par2 <- c(u1, u2, t11, t22, t12, b, a.opt, c11)
    # 
    # names(opt$par2) <- c("u1", "u2", "t11", "t22", "t12", "b", "a", "c11")

    opt$par <- c(u1, u2, t1, t2, r, b, a.opt, c1)
    
    names(opt$par) <- c("u1", "u2", "t1", "t2", "r", "b", "a", "c1")
    
}

  opt

}




