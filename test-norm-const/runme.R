inla.setOption(inla.mode = "experimental")
inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(num.threads = "1:1")
inla.setOption(smtp = "pardiso")

n <- 10^5
x <- rnorm(n, sd = 0.2)
xx <- rnorm(n, sd = 0.2)
p <- 1/(1+exp(-(1+x+xx)))
y <- rbinom(n, prob = p, size = 1)

r <- inla(y ~ 1 + x+xx,
          data = data.frame(y, x, xx),
          Ntrials = 1,
          family = "poisson",
          verbose = TRUE,
          inla.call = "inla.mkl.work")

rr <- inla(y ~ 1 + x+xx,
          data = data.frame(y, x, xx),
          Ntrials = 1,
          family = "poisson",
          verbose = T, 
          inla.call = "inla.mkl")

r$cpu
rr$cpu

r$mlik - rr$mlik


