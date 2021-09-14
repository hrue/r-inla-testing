## this is a quite complicated example...
n = 11
prec.y = 100
prec.obs = 10
prec.x = 1
x = rnorm(n, sd = 1/sqrt(prec.x)) 
xx = x + rnorm(n, sd = 1/sqrt(prec.obs))
y = 1 + 4*x + rnorm(n, sd = 1/sqrt(prec.y))

prec.param = c(1, 0.01)
prior.beta = c(0, 0.1)
prec.intercept = 1

## first spesify the model using the 'copy'-feature; quite involved...
Y = matrix(NA, 2*n, 2)
Y[1:n, 1] = y
Y[1:n + n, 2] = xx
intercept = c(rep(1, n), rep(NA, n))

idx.x = c(rep(NA, n), 1:n)
idx.copy = c(1:n, rep(NA, n))

formula = Y ~ 0 + intercept +
    f(idx.x, values = 1:n, hyper = list(prec = list(initial = log(prec.x), param = prec.param, fixed=TRUE))) +
    f(idx.copy, copy="idx.x", fixed=FALSE, hyper = list(beta = list(param = prior.beta)), precision = 1e9)

r = inla(formula,
        data = list(Y=Y, idx.x=idx.x, idx.copy = idx.copy, intercept = intercept),
        family = c("gaussian", "gaussian"),
        verbose = TRUE,
        control.family = list(
                list(hyper = list(prec = list(initial = log(prec.y), param = prec.param, fixed=TRUE))),
                list(hyper = list(prec = list(initial = log(prec.obs), param = prec.param, fixed=TRUE)))
                ),
        control.inla = list(h = 0.0001, tolerance = 1e-7), 
        control.fixed = list(prec = prec.intercept)
        )

## then using the new 'berkson' model. 
iintercept = rep(1, n)
fformula = y ~ 0 + iintercept +
    f(xx, model="berkson",
      hyper = list(
              beta = list(param = prior.beta, fixed = FALSE),
              prec.obs = list(param = prec.param,  initial = log(prec.obs), fixed = TRUE),
              prec.x = list(param = prec.param,  initial = log(prec.x), fixed = TRUE),
              mean.x = list(initial = 0, fixed=TRUE)))

rr = inla(fformula,  data = data.frame(y, xx, iintercept), family = "gaussian",
        control.family = list(hyper = list(prec = list(param = prec.param, initial = log(prec.y), fixed=TRUE))),
        control.fixed = list(prec = prec.intercept), 
        control.inla = list(h = 0.0001, tolerance = 1e-7), 
        verbose = TRUE,  keep=TRUE)

r = inla.hyperpar(r, diff.logdens = 10)
rr = inla.hyperpar(rr, diff.logdens =10)

rbind(r$summary.hyperpar,rr$summary.hyperpar)
rbind(r$summary.fixed,rr$summary.fixed)
