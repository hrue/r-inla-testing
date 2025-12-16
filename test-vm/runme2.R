g <- function(x) 2 * atan(x)
g.inv <- function(x) tan(x/2)
g.deriv <- function(x) 2 / (1+x^2)
I0 <- function(x) besselI(x, 0)

ud <- function(y, kappa = 1, eta = 0) exp(kappa*cos(g(g.inv(y)-eta)))
C <- function(kappa = 1) 2*pi*I0(kappa)
dens <- function(y, kappa = 1, eta = 0) ud(y, kappa, eta) / C(kappa) * 
	(g.deriv(g.inv(y)-eta) / g.deriv(g.inv(y)))

n <- 1000
x <- rnorm(n, sd = 0.1)
x <- x - mean(x)
eta <- 0.1 + x
kappa <- 10
y <- numeric(n)

for(i in 1:n) {
    yy <- seq(-3, 3, by = 0.01)
    m <- length(yy)
    d <- numeric(m)
    for(j in 1:m) {
        d[j] <- dens(yy[j], kappa, eta[i])
    }
    y[i] <- sample(yy, 1, prob = d)
}



r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "lavm",
          control.inla = list(cmin = 0), 
          control.family = list(link = "tan",
                                hyper = list(
                                    prec = list(initial = log(kappa),
                                                fixed = !TRUE))), 
          verbose = TRUE,
          num.threads = "1:1")
summary(r)
