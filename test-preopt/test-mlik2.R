set.seed(123)
n <- 1
x <- rnorm(n)
s <- 1
y <- 0 + x + rnorm(n, sd = s)
y <- 0 + rnorm(n, sd = s)
y <- 0

fix <- TRUE
##INLA:::inla.my.update()
##inla.setOption(inla.call = "inla.mkl.work", num.threads = "1:1", smtp = 'band')

r <- inla(y ~ 1, data = data.frame(y, x), ##inla.mode = "classic", 
          control.fixed = list(prec.intercept = 1), 
          control.family = list(hyper = list(prec = list(param = c(10/s^2, 10), initial = log(1/s^2), fixed = fix))))
print(r$mlik)

print(dnorm(y, mean = y, sd = 1, log = TRUE))

prec <- exp(inla.models()$predictor$predictor$hyper$theta$initial)
Q <- matrix(0, 3, 3)
Q[1, 1] <- 1
Q[1, 2] <- -1
Q[2, 1] <- -1
Q[2, 2] <- 1 + prec
Q[2, 3] <- -prec
Q[3, 2] <- -prec
Q[3, 3] <- 1 + prec
S <- solve(Q)
print(dnorm(y, mean = 0, sd = sqrt(S[1, 1]), log = TRUE))

Qc <- Q[2:3, 2:3]
library(mvtnorm)
d1 <- dmvnorm(c(0, 0), sigma = solve(Qc), log = TRUE)
d2 <- dmvnorm(c(0, 0, 0), sigma = solve(Q), log = TRUE)
print(c(d1, d2, d2-d1))
