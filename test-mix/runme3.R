n = 10
y = 1:n
E = runif(n)
EE = c(E, E)
param = c(1, 0.1)
hyper = list(prec = list(
                 initial = 4,
                 prior = "loggamma", 
                 param = param, 
                 fixed = FALSE))

yy = n:1

Y = matrix(NA, 2*n, 2)
Y[1:n, 1] = y
Y[1:n + n, 2] = yy


r = inla(
    formula = Y ~ 1, 
    data = list(Y=Y, EE=EE), 
    family = c("poisson", "poisson"), 
    E = EE, 
    control.family = list(
        list(
            control.mix = list(
                model = "gaussian",
                nq = 31,
                hyper = hyper)),
        list()))

rr = inla(
    formula = Y ~ 1 + f(idx, model="iid", hyper = hyper), 
    data = list(Y=Y, EE=EE, idx=c(1:n, rep(NA, n))), 
    family = c("poisson", "poisson"), 
    E = EE)


plot(inla.smarginal(r$internal.marginals.hyperpar[[1]]),
     main = "posterior for log(prec)",
     type="l", lwd=5, col = "red")
lines(inla.smarginal(rr$internal.marginals.hyperpar[[1]]),
      lwd=3, col="yellow")

