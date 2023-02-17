library(mvtnorm)

n <- 10
A <- matrix(rnorm(n^2), n, n)
Q <- A %*% t(A)
S <- solve(Q)
S <- S / exp(mean(log(diag(S))))
Q <- solve(S)

Cmatrix <- inla.as.sparse(Q)
s <- 0.1
y <- c(rmvnorm(1, sigma = S)) + rnorm(n, sd = s)

r <- inla(
    y ~ -1 + f(idx, model = "generic0", Cmatrix = Cmatrix, 
               hyper = list(prec = list(prior = "loggamma",
                                        param = c(1, 1),
                                        initial = 4,
                                        fixed = FALSE))), 
    num.threads = "1:1",
    data = data.frame(y, idx = 1:n),
    control.family =
        list(hyper = list(
                 prec = list(
                     initial = log(1/s^2),
                     fixed = TRUE))))

## pass in the Cmatrix so its a good fit
## for the cgeneric-implementation, so we store 'Cmatrix'
## column-wise, and upper half only (diagonal included)
ii <- Cmatrix@i
Cmatrix@i <- Cmatrix@j
Cmatrix@j <- ii
idx <- which(Cmatrix@i <= Cmatrix@j)
Cmatrix@i <- Cmatrix@i[idx]
Cmatrix@j <- Cmatrix@j[idx]
Cmatrix@x <- Cmatrix@x[idx]

cmodel <- inla.cgeneric.define(model = "inla_cgeneric_generic0_model",
                               shlib = "cgeneric-demo.so",
                               n = n,
                               Cmatrix = Cmatrix)
rc <- inla(
    y ~ -1 + f(idx, model = cmodel), 
    num.threads = "1:1",
    data = data.frame(y, idx = 1:n),
    control.family = list(
        hyper = list(
            prec = list(
                initial = log(1/s^2),
                fixed = TRUE))),
    verbose = TRUE)

print(r$mode$theta - rc$mode$theta)
print(r$mode$x - rc$mode$x)
print(r$mlik - rc$mlik)
