m = inla.models()$latent$ar1$hyper
prec.intern = m$theta1$to.theta(1.0)
rho.intern = m$theta2$to.theta(0.5)

formula = y ~ -1 + f(idx, model="ar1", hyper = list(
                                           prec = list(
                                               initial = prec.intern,
                                               fixed=TRUE),
                                           rho = list(
                                               initial = rho.intern,
                                               fixed=TRUE)))
n = 10
Q = INLA:::inla.extract.Q("idx", formula, data = data.frame(y=rnorm(n), idx=1:n),
                          control.family = list(hyper = list(prec = list(fixed=TRUE))))
print(Q)
solve(Q)


m = inla.models()$latent$rw2$hyper
prec.intern = m$theta$to.theta(1.0)

formula = y ~ -1 + f(idx, model="rw2", hyper = list(
                                           prec = list(
                                               initial = prec.intern,
                                               fixed=TRUE)))

n = 10
Q = INLA:::inla.extract.Q("idx", formula, data = data.frame(y=rnorm(n), idx=1:n),
                          control.family = list(hyper = list(prec = list(fixed=TRUE))))
print(Q)

