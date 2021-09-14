inla.my.update(b=T)

if (FALSE) {
    n = 20
    x = rnorm(n)
    s = 1:n
    y = 1 + x + rnorm(n, sd=0.001)/sqrt(s)

    r  = inla(y ~ 1 + x, family = "gaussian",  data = data.frame(x, y), verbose=TRUE, scale=s, keep=T, weights=NULL, 
            control.inla=list(int.strategy="eb"),
            control.data = list(hyper = list(prec = list(prior="flat", param=numeric(0)))))

    inla.setOption("enable.inla.argument.weights", TRUE)
    rr = inla(y ~ 1 + x, family = "gaussian",  data = data.frame(x, y), verbose=TRUE, weight=s, 
            control.inla=list(int.strategy="eb"), 
            control.data = list(hyper = list(prec = list(prior="flat", param=numeric(0)))))
}


if (TRUE) {
    n = 100

    time = runif(n)
    z = rnorm(n)
    zz = rnorm(n)
    
    formula = inla.surv(time) ~ z + zz
    
    r = inla(formula, family = "coxph", data = data.frame(time,z,zz),
            control.hazard = list(cutpoints = c(0, 0.3,2,3), param=c(1,2)), debug = TRUE, verbose=TRUE, keep=T)

    rr = inla(formula, family = "coxph", data = data.frame(time,z,zz),
            control.hazard = list(cutpoints = c(0, 0.3,2,3), param=c(1,2)), debug = TRUE, verbose=TRUE, keep=T,
            weight = rep(2, n))
}
