inla.setOption(inla.call = NULL,
               num.threads = "4:1")
first.time <- TRUE
source("spde_nsexample.R")
orig <- c(fitns0$cpu[2], 
          fitns1$cpu[2], 
          fitns2$cpu[2])
orig.mlik <- c(fitns0$mlik, 
               fitns1$mlik, 
               fitns2$mlik)

inla.setOption(inla.call = 'inla.mkl.work')
first.time <- FALSE
source("spde_nsexample.R")
new <- c(fitns0$cpu[2], 
          fitns1$cpu[2], 
          fitns2$cpu[2])
new.mlik <- c(fitns0$mlik, 
              fitns1$mlik, 
              fitns2$mlik)

orig
new
new.mlik-orig.mlik

