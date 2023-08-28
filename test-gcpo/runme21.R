n <- 10
S <- matrix(rnorm(n^2), n, n)
S <- S %*% t(S)
Q <- solve(S)
Q.post <- Q + diag(n)
S.post <- solve(Q.post)

y <- rep(0, n)
r <- inla(
    y ~ -1 + f(idx, model = "generic0",
               Cmatrix = Q,
               hyper = list(prec = list(initial = 0, fixed = TRUE))),
    control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))),
    data = data.frame(y, idx = 1:n),
    control.compute = list(
        config = TRUE, 
        control.gcpo = list(num.level.sets = -1, enable = TRUE)))

for(i in 1:n) {
    d <- diag(n)
    d[i, i] <- 0
    Q.i <- Q + d
    S.i <- solve(Q.i)
    var.i.1 <- diag(S.i)[i]
    var.i.2 <- as.vector(r$misc$configs$config[[1]]$gcpodens.moments[i, "variance"])
    var.ref <- diag(S.post)[i]
    print(c(i, var.i.1, var.i.2))
}
