library(runjags)

data(Germany)
Germany <- Germany[1:50, ]

r <- run.jags(model = "~/p/inla/testing/test-vb/jagscode.R",
              data = list(y = Germany$Y, N = dim(Germany)[1], E = Germany$E, tau = 5), 
              monitor = c("intercept"),
              method = "parallel",
              sample = 10000, 
              n.chains = 10)
intercept <- combine.mcmc(r)[, 1]

