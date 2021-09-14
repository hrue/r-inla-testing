`rgeneric.test` = function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
    theta = NULL)
{
    graph = function() return(matrix(1, n, n))
    Q = function() {
        Q <- matrix((1:n)-mean(1:n), n, n)
        Q <- Q %*% t(Q)
        diag(Q) <- diag(Q)+n
        return(inla.as.sparse(Q))
    }
    mu = function() return (numeric(0))
    log.norm.const = function() return (numeric(0))
    log.prior = function() return(0)
    initial = function() return(numeric(0))
    val = do.call(match.arg(cmd), args = list())
    return (val)
}

`rgeneric.test.opt` = function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
    theta = NULL)
{
    graph = function() {
        A <- matrix(1, n, n)
        A[lower.tri(A)] <- 0
        return(inla.as.sparse(A))
    }
    Q = function() {
        Q <- matrix((1:n)-mean(1:n), n, n)
        Q <- Q %*% t(Q)
        diag(Q) <- diag(Q)+n
        Q[lower.tri(Q)] <- 0
        Q <- inla.as.sparse(Q)
        return(Q@x)
    }
    mu = function() return (numeric(0))
    log.norm.const = function() return (numeric(0))
    log.prior = function() return(0)
    initial = function() return(numeric(0))
    val = do.call(match.arg(cmd), args = list())
    return (val)
}

inla.setOption(inla.call = 'inla.valgrind')
inla.setOption(inla.call = 'inla.work')
inla.setOption(num.threads = "2:2")

n = 6
s = 1
y = rnorm(n, sd = 1)
Q <- rgeneric.test("Q")
ld <- 0.5 * log(det(as.matrix(Q)))

##rr <- sample(1:nr, n, replace = TRUE)
##gg <- sample(1:ng, n, replace = TRUE)

r1 = inla(y ~ -1 + f(idx, model="generic", Cmatrix = Q, hyper = list(prec = list(initial = 0, fixed = TRUE))), 
          data = data.frame(y = y, idx = 1:n), 
          control.family = list(hyper = list(prec = list(initial=log(1/s^2), fixed=TRUE))))
r1$mlik <- r1$mlik + ld

model2 = inla.rgeneric.define(rgeneric.test, n=n, debug=FALSE, optimize = FALSE)
inla.rgeneric.q(model2, "Q")
r2 = inla(y ~ -1 + f(idx, model=model2), 
          data = data.frame(y = y, idx = 1:n), 
          verbose = TRUE, 
          control.family = list(hyper = list(prec = list(initial=log(1/s^2), fixed=TRUE))))


model3 = inla.rgeneric.define(rgeneric.test.opt, n=n, debug=FALSE, optimize = TRUE)
inla.rgeneric.q(model3, "Q")
r3 = inla(y ~ -1 + f(idx, model=model3), 
          data = data.frame(y = y, idx = 1:n), 
          verbose = TRUE, 
          control.family = list(hyper = list(prec = list(initial=log(1/s^2), fixed=TRUE))))

print(r1$mlik - r2$mlik)
print(r1$mlik - r3$mlik)
print(r2$mlik - r3$mlik)

print(r1$cpu)
print(r2$cpu)
print(r3$cpu)
