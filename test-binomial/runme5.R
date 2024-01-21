n <- 100000
x <- rnorm(n, sd = 0.5)
off <- rnorm(n, sd = 0.1)
eta <- 4 + x + off
p <- 1.0/(1+exp(-eta))
size <- 10
y <- rbinom(n, prob = p, size = size)

inla.setOption(num.threads = "1:1",
               safe = FALSE,
               verbose = !TRUE)

r <- inla(y ~ 1 + x + offset(off),
          family = "binomial",
          Ntrials = size, 
          control.compute = list(cpo = T),
          data = data.frame(y, x, off, size))

rr <- inla(y ~ 1 + x + offset(off),
           family = "binomial",
           Ntrials = size, 
           verbose = T, 
           keep = T, 
           control.compute = list(cpo = T), 
           data = data.frame(y, x, off, size),
           inla.call = "inla.mkl.work")

r$logfile[grep("stage1", r$logfile)]
rr$logfile[grep("stage1", rr$logfile)]

r$summary.fixed - rr$summary.fixed
mean(abs(r$cpo$pit - rr$cpo$pit))

r$mlik - rr$mlik
