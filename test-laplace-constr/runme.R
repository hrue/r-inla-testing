n = 5
y = rep(1,n)
x = 1:n
nc=2
A = matrix((1:(nc*n))^2, nc, n)
A = matrix(runif(nc*n),nc,n)
print(A)
e = runif(nc)
formula = y ~ f(x, model="iid", extraconstr=list(A=A,e=e), initial=0, fixed=TRUE) -1
inla.setOption("inla.call", "inla.work")
inla.setOption("num.threads", 1)

r = inla(formula, data = data.frame(x,y),
        control.predictor = list(initial = log(1000), fixed=TRUE, compute=T),
        control.family = list(initial=0,fixed=TRUE),
        control.compute = list(smtp="band"),
        control.inla = list(strategy = "gaussian", int.strategy="eb",
                reordering = "identity"),verbose=T, keep=TRUE)
rr = inla(formula, data = data.frame(x,y),
        control.predictor = list(initial = log(1000), fixed=TRUE, compute=TRUE),
        control.family = list(initial=0,fixed=TRUE),
        control.compute = list(smtp="band"),
        control.inla = list(strategy = "laplace", int.strategy="eb",
                npoints=3, reordering = "identity"),verbose=T, keep=TRUE)

r$summary.random$x$mean
rr$summary.random$x$mean
r$summary.random$x$mean - rr$summary.random$x$mean

r$summary.random$x$sd
rr$summary.random$x$sd
r$summary.random$x$sd/rr$summary.random$x$sd

