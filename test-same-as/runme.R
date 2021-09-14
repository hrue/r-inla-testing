n = 10
y = c(1:n, 1:n, 2*(1:n))

x = c(1:n, rep(NA, 2*n))
xx = c(rep(NA,n), 1:n, rep(NA, n))
xxx = c(rep(NA, 2*n), 1:n)
www = c(rep(NA,2*n),rep(2,n))

formula = y ~ f(x) + f(xx, copy="x", fixed=FALSE) + f(xxx, www, copy="x", same.as = "xx")
r = inla(formula, data = data.frame(y,x,xx,xxx),verbose=TRUE,
        control.predictor =list(compute=TRUE),
        control.family = list(initial=4,fixed=T))

