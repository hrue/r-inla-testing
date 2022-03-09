make.S <- function(n) {
    A <- matrix(rnorm(n^2), n, n)
    S <- A %*% t(A)
    S <- S / sum(diag(S))
    return(S)
}
    
inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(smtp = "band")
inla.setOption(num.threads = "1:1")
set.seed(123)

n <- 5
m <- 5
nc <- 2
Ac <- matrix(rnorm(nc*n), nc, n)
ec <- rnorm(nc)

Q1 <- solve(make.S(n))
Q2 <- solve(make.S(n))
Q3 <- solve(make.S(n))

S <- solve(Q1)## + solve(Q2) + solve(Q3)
##S <- solve(Q1)
D.prec <- diag(1/1:m)
Ap <- matrix(rnorm(m*n), m, n)

## transfer this to an eqv constraint on eta*
AAc <- Ac %*% solve(Ap)
eec <- ec
SD <- solve(solve(Ap %*% S %*% t(Ap)) + D.prec)
SD <- SD - SD %*% t(AAc) %*% solve(AAc %*% SD %*% t(AAc)) %*% AAc %*% SD
cov.lp <- SD
print(round(dig = 4, cov.lp))

r <- inla(
    y ~ -1 +
        f(i1, model = "generic", Cmatrix = Q1,
          hyper = list(prec = list(initial = 0, fixed = TRUE)), 
          extraconstr = list(A = Ac, e = ec)), 
    family = "gaussian",
    control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))),
    scale = 1/1:m, 
    data = list(y = rep(0, m), i1 = 1:n, i2 = 1:n, i3 = 1:n, m = m, Ap = Ap),
    control.compute = list(
        control.gcpo = list(
            enable = TRUE, 
            verbose = TRUE, 
            group.size = m)), 
    control.predictor = list(A = Ap), 
    inla.mode = "experimental", 
    verbose = TRUE)

print(round(dig = 8, cov.lp))
