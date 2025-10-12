d <- function(rho) {
    gamma1 <- 2*rho / (1+rho^2)
    gamma2 <- (3*rho^2 - rho^4) / (1 + rho^2)
    return (sqrt(3 - 4*gamma1 + gamma2))
}
jac <- function(rho) {
    return ((4 - 4 * rho^2 - 3 * rho + 2 * rho^3 + rho^5) /
            ( d(rho) * (1+rho^2)^2 ))
}

## note that d(0) = sqrt(3)
dens <- function(rho, lambda = 1) {
    return ((lambda * exp(-lambda * d(rho)) / (1-exp(-lambda * d(0)))) * jac(rho))
}

eps <- 1e-4
rho <- seq(eps, 1-eps, by = eps)
lambda <- 10
par(mfrow = c(1, 2))
plot(rho, dens(rho, lambda))

## this should be close to 1
sum((c(diff(rho), 0) + c(0, diff(rho)))/2  * dens(rho, 1))


## rho = exp(-sqrt(12) / range)
jacc <- function(range) exp(-sqrt(12)/range) * (sqrt(12)/range^2)
rho2range <- function(rho) 1/(-log(rho) / sqrt(12))

r <- rho2range(rho)
dd <- dens(rho, lambda) * jacc(r)
plot(r,  dd, xlim = c(0, 300))
abline(h = 0)

if (FALSE) {
    s <- 10^6
    rrho <- unique(floor(s * rho))
    ddens <- dens(rrho / s,  lambda)
    r.sample <- rho2range(sample(rrho, 100000,  prob = ddens, replace = TRUE) / s)
    hist(r.sample, xlim=c(0,200), prob=T,n=6000, add = T)
}
