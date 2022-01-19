##******************************************************************************
##
## 1000 TIMES SIMULATION EXPERIMENT
##
## SAVE DATA INTO DIFFERENT FOLDERS IN results-RData-Rmd/t12 or results-RData-Rmd/t0.7
##
##  
##******************************************************************************


## 1. LOAD PKGS

library("mixmeta")
library("foreach")
library("parallel")
library("doSNOW")
library("doRNG")


## 2. LOAD SIMULATION FUNCTIONS

rm(list = ls())
.files.sources <- list.files("../R/")
sapply(paste0("../R/", .files.sources), source)


## 3. CHOOSE ONE OF THE scenariosS


# tset <- "t12"  ## SMALL TAU
# tset <- "t0.7"   ## LARGE TAU

## 4. 1000 TIMES REPEAT FOR 3 SETTING OF C=(c11, c22)

re <- 1000


##******************************************************************************
##
## 5 MODELS COMPARISON: PROPOSED.c, PROPOSED (c1=c2), PROPOSED (c1=1), Reitsma.O, Reistma.P
## TRUE c: c11=c22=0.5
##
## RESULTS DATA SAVE INTO /c11/
## FOLDER 1 ----
##
##******************************************************************************

ncores <- detectCores()
cl <- makeCluster(ncores, "SOCK")
doSNOW::registerDoSNOW(cl)


t <- proc.time()


load(paste0("scenarios/scenarios-",tset,"/set-", tset, "-c11.RData"))

folder1 <- paste0("results-RData-Rmd/",tset,"/c11/")


set.seed(2021)

for(list.n in c(1:4)){  ## i list  # Sample Size
  
  for(row.n in c(1:6)){ ## j row  # scenarios 
    
    DATA <- foreach(r=1:re, .combine = "cbind", .packages = c("mixmeta"))  %dorng%  {

      simu(list.n, 
           row.n,
           beta0 = 1,
           csq0 = 0.5,   ## SA2: INITIAL VALUE OF c11
           csq.w = 1,    ## SA1: MIS-SPECIFIED VALUE OF c11
           beta.interval = c(0, 2),
           alpha.interval = c(-2, 2)
           )
    }
    save(DATA, file = paste0(folder1, "/sim_l", list.n, "_r", row.n, ".RData"))  
  }
}


# parallel::stopCluster(cl)



##******************************************************************************
##
## 5 MODELS COMPARISON: PROPOSED.c, PROPOSED (c1=1), PROPOSED (c1=c2), Reitsma.O, Reistma.P
## TRUE c: c11=1, c22=0
##
## RESULTS DATA SAVE INTO /c10/
## FOLDER 2 ----
##
##******************************************************************************

load(paste0("scenarios/scenarios-",tset,"/set-", tset, "-c10.RData"))

folder2 <- paste0("results-RData-Rmd/",tset,"/c10/")


# ncores <- detectCores()
# cl <- makeCluster(ncores, "SOCK")
# doSNOW::registerDoSNOW(cl)
# 
set.seed(2021)

for(list.n in c(1:4)){  ## i list  # Sample Size
  
  for(row.n in c(1:6)){ ## j row  # scenarios 
    
    DATA <- foreach(r=1:re, .combine = "cbind", .packages = c("mixmeta"))  %dorng%  {
      
      simu(list.n, 
           row.n,
           beta0 = 1,
           csq0 = 0.5,   ## SA2: INITIAL VALUE OF c11
           csq.w = 0.5,  ## SA1: MIS-SPECIFIED VALUE OF c11
           beta.interval = c(0, 2),
           alpha.interval = c(-2, 2))
    }
    save(DATA, file = paste0(folder2, "/sim_l", list.n, "_r", row.n, ".RData"))  
  }
}


# parallel::stopCluster(cl)



##******************************************************************************
##
## 5 MODELS COMPARISON: PROPOSED.c, PROPOSED (c1=0), PROPOSED (c1=c2), Reitsma.O, Reistma.P
## TRUE c: c11=0, c22=1
##
## RESULTS DATA SAVE INTO /c01/
## FOLDER 3 ----
##
##******************************************************************************

load(paste0("scenarios/scenarios-",tset,"/set-", tset, "-c01.RData"))

folder3 <- paste0("results-RData-Rmd/",tset,"/c01/")


# ncores <- detectCores()
# cl <- makeCluster(ncores, "SOCK")
# doSNOW::registerDoSNOW(cl)
# 
set.seed(2021)

for(list.n in c(1:4)){  ## i list  # Sample Size
  
  for(row.n in c(1:6)){ ## j row  # scenarios 
    
    DATA <- foreach(r=1:re, .combine = "cbind", .packages = c("mixmeta"))  %dorng%  {
      
      simu(list.n, 
           row.n,
           beta0 = 1,
           csq0 = 0.5,   ## SA2: INITIAL VALUE OF c11
           csq.w = 0.5,  ## SA1: MIS-SPECIFIED VALUE OF c11
           beta.interval = c(0, 2),
           alpha.interval = c(-2, 2))
    }
    save(DATA, file = paste0(folder3, "/sim_l", list.n, "_r", row.n, ".RData"))  
  }
}


parallel::stopCluster(cl)



proc.time()-t

##
##     user   system  elapsed 
##    83.94    18.31 12483.35 
##
