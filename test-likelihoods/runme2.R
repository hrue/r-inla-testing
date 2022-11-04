library(INLA)
inla.setOption(inla.mode = "experimental")
inla.setOption(inla.call = "inla.mkl.work")

n <- 2000
N <- 2*n

size <- 100
Y <- matrix(NA, N, 2)
Y[1:n, 1] <- rbinom(n, size = size, prob = 0.4)
Y[n + 1:n, 2] <- rbinom(n, size = size, prob = 0.6)

p <- as.factor(rep(1:2, each = n))
r <- inla(Y ~ -1 + p, 
          data = list(Y = Y, p = p, size = size),
          Ntrials = size, 
          control.compute = list(config = TRUE, residuals = TRUE), 
          family = rep("binomial", 2))

if (FALSE) {
    x <- inla.posterior.sample(10000, r)
    pdiff <- inla.posterior.sample.eval(
        function() return(inla.link.invlogit(p1) -
                          inla.link.invlogit(p2)),
        x)
    hist(pdiff, n = 300, cex = 1)
}
