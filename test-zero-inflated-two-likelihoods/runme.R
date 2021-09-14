n <- 100
x <- rnorm(n, sd = 0.3)
eta.z <- -2+x
prob <- 1/(1+exp(-eta.z))
z <- rbinom(n, size = 1, prob = prob)

xx <- rnorm(n, sd = 0.3)
eta.y <- 2+xx
y <- numeric(n)
for(i in 1:n) {
    while(TRUE) {
        y[i] <- rpois(1, lambda = exp(eta.y[i]))
        if (y[i] > 0)
            break
    }
}

## if z == 1, we have a zero from the zero-component, meaning, we have not observed what the
## non-zero y is as its not there. if z==0, we observe also a y > 0
y[z == 1] <- NA

## we can make this more efficient, if z==1, we do not need to know the corresponding y nor do
## we need to know the covariates in that case. I'll do that after the straight forward option
na <- rep(NA, n)
X <- c(x, na)
XX <- c(na, xx)
intercept <- as.factor(rep(1:2, each = n))
Y <- cbind(c(z, na), c(na, y))

formula <- Y ~ -1 + intercept + X + XX
r <- inla(formula,
          data = list(Y = Y, X = X, XX = XX),
          family = c("binomial", "poisson"))


## remove the part do we do know/need from the model
na <- rep(NA, n)
idx.z.zero <- which(z == 0)
m <- length(idx.z.zero)
ma <- rep(NA, m)
X <- c(x, ma)
XX <- c(na, xx[idx.z.zero])
intercept <- as.factor(c(rep(1, n), rep(2, m)))
Y <- cbind(c(z, ma), c(na, y[idx.z.zero]))

fformula <- Y ~ -1 + intercept + X + XX
rr <- inla(fformula,
          data = list(Y = Y, X = X, XX = XX),
          family = c("binomial", "poisson"))

## the result is the same though
print(r$mlik - rr$mlik)

