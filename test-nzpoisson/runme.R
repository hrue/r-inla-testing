n <- 100
a <- 1
b <- 0.2
z <- rnorm(n)
eta <- a + b*z
E <- runif(n)
off <- rnorm(n)
lambda <- E * exp(eta + off)
y <- numeric(n)
for(i in 1:n) {
    while((y[i] <- rpois(1, lambda[i])) == 0) TRUE
}

r <- inla(y ~ 1 + z + offset(off),
          family = "nzpoisson",
          data = data.frame(y, z, off),
          E=E)
rr <- inla(y ~ 1 + z + offset(off),
           family = "nzpoisson",
           data = data.frame(y, z, off),
           E=E,  inla.call = "inla.mkl.work")


