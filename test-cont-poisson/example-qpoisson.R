n = 1000
intercept = 3
x = 0.3 * scale(rnorm(n))
beta = 1
eta = intercept + beta * x
E = runif(n, min=0, max=2)
E[] = 1
lambda = E*exp(eta)
u = runif(n)
y = numeric(n)

for(i in 1:n) {
    y[i] = inla.qcontpoisson(u[i], lambda[i])
}

r = inla(y ~ 1 + x,
         data = data.frame(y, x, E),
         family = "contpoisson",
         control.family = list(quantile = alpha),
         E =E,
         verbose=T,
         keep=F)
summary(r)

alpha = 0.9
for(i in 1:n) {
    lambda[i] = E[i] * inla.contpoisson.solve.lambda(exp(eta[i]), alpha = alpha)
    y[i] = inla.qcontpoisson(u[i], lambda[i])
}

rr = inla(y ~ 1 + x,
          data = data.frame(y, x, E),
          family = "qcontpoisson",
          control.family = list(quantile = alpha),
          E =E,
          verbose=T, keep=T)
summary(rr)

