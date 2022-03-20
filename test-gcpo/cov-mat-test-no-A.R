make.S <- function(n) {
    A <- matrix(rnorm(n^2), n, n)
    S <- A %*% t(A)
    S <- S / sum(diag(S))
    return(S)
}
    
##inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(smtp = "band")
inla.setOption(num.threads = "1:1")
##set.seed(123)

n <- 5
m <- 5
nc <- 2
Ac <- matrix(rnorm(nc*n), nc, n)
ec <- 1+rnorm(nc)

##set.seed(1234)
Q1 <- solve(make.S(n))
Q2 <- solve(make.S(n))
Q3 <- solve(make.S(n))

S <- solve(Q1)
S <- solve(Q1)
D.prec <- diag(m)
diag(D.prec) <- 1/1:m

SD <- solve(solve(S) + D.prec)
SD <- SD - SD %*% t(Ac) %*% solve(Ac %*% SD %*% t(Ac)) %*% Ac %*% SD
cov.lp <- SD

r <- inla(
    y ~ -1 +
        f(i1, model = "generic", Cmatrix = Q1,
          hyper = list(prec = list(initial = 0, fixed = TRUE)), 
          extraconstr = list(A = Ac, e = ec)), 
    family = "gaussian",
    scale = 1/1:m, 
    control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))),

    data = list(y = 1:m, i1 = 1:n, i2 = 1:n, i3 = 1:n, m = m, Ap = Ap),
    control.compute = list(
        control.gcpo = list(
            enable = TRUE, 
            verbose = TRUE, 
            group.size = m)), 
    inla.mode = "experimental", 
    verbose = TRUE)

print(round(dig = 8, cov.lp))
