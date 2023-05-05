INLA:::inla.my.update(bin = TRUE)
inla.setOption(num.threads = 1)

if (!exists("y")) {
    n = 50
    rho = 0.9
    x = arima.sim(n=n, model = list(ar = rho))
    x = x - mean(x)
    z = rnorm(n, sd = 0.3)
    s <- 0.1
    y = rpois(n, exp(1 + z + x))
    idx = 1:n
}

formula = y ~ 1 + z + f(idx, model="ar1", diagonal = 0)
r = inla(formula,  data = data.frame(y, z, idx),
         family = "poisson", 
         control.fixed = list(prec.intercept = 1, prec = 1), 
         control.compute = list(config = TRUE, internal.opt = FALSE),
         control.inla = list(int.strategy = "eb",
                             control.vb = list(strategy = "mean")))
rr = inla(formula,  data = data.frame(y, z, idx),
         family = "poisson", 
         control.fixed = list(prec.intercept = 1, prec = 1), 
         control.compute = list(config = TRUE, internal.opt = FALSE),
         control.inla = list(int.strategy = "eb",
                             control.vb = list(strategy = "variance")))
a=r$misc$configs$config[[1]]
aa=rr$misc$configs$config[[1]]
diag(a$Q)[n:(n+2)]
diag(aa$Q)[n:(n+2)]
diag(aa$Q)[n:(n+2)]-diag(a$Q)[n:(n+2)]

QQ <- aa$Q + t(aa$Q)
diag(QQ) <- diag(QQ)/2.0
QQinv <- inla.qinv(QQ)

