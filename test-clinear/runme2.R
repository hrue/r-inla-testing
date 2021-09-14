n = 50
s = 0.1
beta = 2
x = rnorm(n)
y = 1 + beta*x + rnorm(n, sd=s)
r = inla(y ~ 1 + f(x, model="clinear", range = c(0, Inf)),
        data = data.frame(y, x),
        verbose=TRUE, 
        family = "t",
        control.predictor = list(hyper = list(prec = list(initial = 20))), 
        control.inla = list(cmin = 0, int.strategy = "eb"), 
        control.fixed = list(prec = 1, prec.intercept = 1), 
        control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed=FALSE), 
                                           dof = list(initial = 4, fixed = TRUE))),
        inla.call = "inla",
        inla.arg = "-v -t1:1 -b -i")
rr = inla(y ~ 1 + f(x, model="clinear", range = c(0, Inf)),
        data = data.frame(y, x),
        verbose=TRUE,
        family = "t",
        control.predictor = list(hyper = list(prec = list(initial = 20))), 
        control.inla = list(cmin = 0, int.strategy = "eb"), 
        control.fixed = list(prec = 1, prec.intercept = 1), 
        control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed=FALSE), 
                                           dof = list(initial = 4, fixed = TRUE))),
        inla.call = "inla.mkl.work",
        inla.arg = "-v -t1:1 -b -P -i")
        
summary(r)
summary(rr)

r$mlik
rr$mlik
as.numeric(r$mode$theta)
as.numeric(rr$mode$theta)

r$.args$verbose <- FALSE
rr$.args$verbose <- FALSE
r <- inla.rerun(r)
r <- inla.rerun(r)
rr <- inla.rerun(rr)
rr <- inla.rerun(rr)

r$mlik
rr$mlik
as.numeric(r$mode$theta)
as.numeric(rr$mode$theta)
