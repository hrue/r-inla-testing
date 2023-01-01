n <- 1000
x <- rnorm(n)
eta <- 0 + 0.3 * x
size <- 5
y <- rbinom(n, size = size, prob = 1/(1+exp(-eta)))

r <- inla(y ~ 1 + x,
          family = "binomial",
          Ntrials = size, 
          data = data.frame(y, x, size))
rr <- inla(y ~ 1 + x,
           family = "binomial",
          Ntrials = size, 
           data = data.frame(y, x, size),
           inla.call = "inla.mkl.work")
r <- inla(y ~ 1 + x,
          family = "binomial",
          Ntrials = size, 
          control.compute = list(dic = TRUE), 
          data = data.frame(y, x, size))
rr <- inla(y ~ 1 + x,
          family = "binomial",
          Ntrials = size, 
          control.compute = list(dic = TRUE), 
          data = data.frame(y, x, size),
          inla.call = "inla.mkl.work")
r$mlik - rr$mlik
max(abs(rr$dic$local.dic - r$dic$local.dic))
r$logfile[grep("stage1", r$logfile)]
rr$logfile[grep("stage1", rr$logfile)]

