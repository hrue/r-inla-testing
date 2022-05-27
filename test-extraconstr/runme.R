INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(inla.mode = "experimental")
inla.setOption(num.threads = "1:1")

n <- 50
idx <- 1:n
eta <- sin(idx/2)
y <- eta + rnorm(n, sd = 0.1)

m <- 3
A <- matrix(rnorm(n*m) * rbinom(n*m, size = 1, prob = 0.5), m, n)
e <- rep(0, m)
extraconstr <- list(A = A, e = e)

r <- inla(y ~ -1 + f(idx, model = "rw2", scale.model = TRUE,
                     constr = TRUE, 
                     extraconstr = extraconstr), 
          data = data.frame(idx, y),
          verbose = TRUE)

extraconstr$A <- inla.as.sparse(extraconstr$A)
rr <- inla(y ~ -1 + f(idx, model = "rw2", scale.model = TRUE,
                      constr = TRUE, 
                      extraconstr = extraconstr), 
           data = data.frame(idx, y),
           verbose = TRUE)

r$mlik - rr$mlik
