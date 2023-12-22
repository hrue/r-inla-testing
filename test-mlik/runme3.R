kappa.mu = exp(rnorm(1))
kappa.eta = exp(20)
kappa.y = exp(rnorm(1))
y = rnorm(1)
## joint with y
Q = matrix(c(
        kappa.y, -kappa.y, 0,
        -kappa.y, kappa.y + kappa.eta, -kappa.eta,
        0,         -kappa.eta, kappa.eta + kappa.mu), 3, 3)
## conditional on y
Q.y = Q[2:3, 2:3]
b = c(kappa.y * y, 0)
e = solve(Q.y) %*% b
var.y = solve(Q)[1, 1]

r = inla(y ~ 1,
        data = data.frame(y),
        control.fixed = list(prec.intercept = kappa.mu),
        control.predictor = list(hyper = list(prec = list(initial = log(kappa.eta)))),
        control.family = list(hyper = list(prec = list(initial = log(kappa.y), fixed=TRUE))))

## mean is the marginal mean for y,  which is 0
true = dnorm(x=y, mean=0, sd = sqrt(var.y),  log=TRUE) 
est = r$mlik[1]
print(c(true, est, true-est))
