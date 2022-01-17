##******************************************************************************
## SIMULATION EXPERIMENT BASED ON POPULATION DATA ONLY
##
## AIM: TO CALCULATE ALPHA AND P
##  
##******************************************************************************


library("foreach")
library("parallel")
library("doSNOW")
library("doRNG")
source("../../simfun/sim.pdata.R")

# sETTINGS ----

re <- 1000


# CALCULATE ALPHAS ----


##******************************************************************************
##
##  FUNCTION TO CALCULATE ALPHAS, GIVEN P ----
##
##******************************************************************************

b  <- set[[1]][1,9]


fa <- function(a) mean(pnorm(a + b*pdata$t.clnDOR)) - 0.7

a <- NULL


ncores <- detectCores()
cl <- makeCluster(ncores, "SOCK")
doSNOW::registerDoSNOW(cl)

set.seed(2021)

for(i in 1:6){
  
	a1 <- foreach(r=1:re, .combine = "c")  %dorng%  {
		
		pdata <- sim.pdata(set[[3]][i,1:8])
		
		uniroot(fa, c(-3,3), extendInt="yes")$root
	}

	a <- c(a, mean(a1))	
	
}


parallel::stopCluster(cl)

a



##******************************************************************************
##
## FUNCTION TO CHECK P, GIVEN ALPHA ----
##
##******************************************************************************


ncores <- detectCores()
cl <- makeCluster(ncores, "SOCK")
doSNOW::registerDoSNOW(cl)

set.seed(2021)

p <- NULL

for(i in 1:6){
  
  p1 <- foreach(r=1:re, .combine = "c")  %dorng%  {
    
    pdata <- sim.pdata(set[[3]][i,1:8])
    
    mean(pnorm(set[[3]][i,10] + set[[3]][i,9]*pdata$t.clnDOR))
    
  }
  
  p <- c(p, mean(p1))	
  
}


parallel::stopCluster(cl)

p
