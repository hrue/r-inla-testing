inla.setOption(inla.mode = "experimental")
inla.setOption(num.threads = "1:1")
inla.setOption(smtp = "pardiso")

n <- 10^6
x <- rnorm(n, sd = 0.2)
xx <- rnorm(n, sd = 0.2)
p <- 1/(1+exp(-(1+x+xx)))
y <- rbinom(n, prob = p, size = 1)
lambda <- exp(1+x+xx)
y <- rpois(n, lambda = lambda)

if (FALSE) {
    r <- inla(y ~ 1 + x+xx,
              data = data.frame(y, x, xx),
              Ntrials = 1,
              family = "binomial",
              inla.mode = "experimental", 
              verbose = TRUE,
              safe = F, 
              inla.call = "inla.mkl.work")

    rr <- inla(y ~ 1 + x+xx,
               data = data.frame(y, x, xx),
               Ntrials = 1,
               family = "binomial",
               verbose = T, 
               safe = F, 
               inla.call = "inla.mkl")
} else {
    r <- inla(y ~ 1 + x+xx,
              data = data.frame(y, x, xx),
              family = "poisson",
              inla.mode = "experimental", 
              verbose = TRUE,
              safe = F, 
              inla.call = "inla.mkl.work")

    rr <- inla(y ~ 1 + x+xx,
               data = data.frame(y, x, xx),
               Ntrials = 1,
               family = "poisson",
               verbose = T, 
               safe = F, 
               inla.call = "inla.mkl")
}


r$cpu
rr$cpu

r$mlik - rr$mlik


