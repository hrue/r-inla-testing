d <- function(rho) {
    lrho <- log1p(rho-1)
    return (sqrt(-(3+rho)* expm1(lrho)^3 / (1+rho^2)))
}
range2rho <- function(range) exp(-sqrt(12) / range)

r <- rho2range(rho)
dd <- dens(rho, lambda) * jacc(r)
plot(r,  dd, xlim = c(0, 300),  log = "y")
abline(h = 0)

F.range <- function(range,  lambda = 1) {
    return (exp(-lambda * d(range2rho(range))) / (1-exp(-lambda * sqrt(3))))
}

## find lambda so that Prob(range > range0) = alpha
fun.opt2 <- function(lambda,  range0, alpha) {
    ((1-F.range(range0, lambda)) - alpha)^2
}

lambda.star <- optim(1, gr = NULL, fun.opt2, method = "BFGS", range0 = 10, alpha = 0.5, step = 1)$par

rr <- seq(0.001, 25*range0, by = range0/1000)
h <- 1e-4
step <- 1
dd <- (F.range(rr*exp(h), lambda = lambda.star) -
       F.range(rr*exp(-h), lambda = lambda.star)) / (2*h) / rr
