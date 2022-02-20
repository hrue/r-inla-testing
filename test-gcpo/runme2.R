library(mvtnorm)
n <- 10
nc <- 4
AA <- matrix(rnorm(n^2), n, n)
QQ <- AA %*% t(AA)
SS <- solve(QQ)
constr <- list(A = matrix(rnorm(nc*n), nc, n), e = rep(0, nc))

Am <- matrix(rnorm(n^2), n, n)

Sys.setenv(INLA_gcpo = 1)

eta <- Am %*% t(rmvnorm(1, sigma = SS))
eta <- eta - SS %*% t(constr$A) %*%
    solve(constr$A %*% SS %*% t(constr$A)) %*% (constr$A %*% eta - constr$e)
prec <- 10
y <- eta + rnorm(n, sd = sqrt(1/prec))

rr <- inla(y ~ -1 + f(idx, model = "generic", Cmatrix = QQ,
                      extraconstr = constr, 
                      hyper = list(prec = list(initial = 0,
                                               fixed = TRUE))),
           data = data.frame(y, idx = 1:n), 
           family = "gaussian",
           control.family = list(hyper = list(prec = list(initial = log(prec),
                                                          fixed = TRUE))), 
           control.predictor = list(compute = TRUE, A = Am),
           control.compute = list(config = TRUE), 
           inla.mode = "experimental",
           num.threads = "1:1")

Q <- rr$misc$configs$config[[1]]$Q
d <- diag(Q)
Q <- Q + t(Q)
diag(Q) <- d
S <- solve(Q)
S <- S - S %*% t(constr$A) %*%
    solve(constr$A %*% S %*% t(constr$A)) %*% constr$A %*% S
S <- Am %*% S %*% t(Am)

r <- inla(y ~ -1 + f(idx, model = "generic", Cmatrix = QQ,
                     extraconstr = constr, 
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE))),
          data = data.frame(y, idx = 1:n), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(prec),
                                                         fixed = TRUE))), 
          control.predictor = list(compute = TRUE, A = Am),
          control.compute = list(config = TRUE), 
          inla.mode = "experimental",
          num.threads = "1:1",
          inla.call = "inla.mkl.work", 
          ##inla.call = "inla.valgrind", 
          keep = T, 
          verbose = TRUE)

print(round(dig = 8, diag(S)))
print(round(dig = 8, S[1, ]))

D <- diag(1/sqrt(diag(S)))
print(round(dig = 8, D %*% S %*% D))
