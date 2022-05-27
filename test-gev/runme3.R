rgev = function(n=1, xi = 0, mu = 0.0, sd = 1.0) {
    u = runif(n)
    if (xi == 0) {
        x = -log(-log(u))
    } else {
        x = ((-log(u))^(-xi) - 1.0)/xi
    }
    return (x*sd + mu)
}

n = 10000
xi = -0.3
xi.scale = 0.1
x <- rnorm(n)
y = 1 + x+rgev(n, xi=xi)

INLA:::inla.my.update()
inla.setOption(inla.mode = "experimental")
inla.setOption(inla.call = "inla.mkl.work")

## hack to remove the 'disabled' restriction
m <- get("inla.models", envir = inla.get.inlaEnv())
m$likelihood$gev$status <- NULL
assign("inla.models", m, envir = inla.get.inlaEnv())

formula = y ~ 1 + x
data = data.frame(y, x)

r = inla(formula,
         data = data,
         family = "gev",
         verbose=TRUE, 
         control.fixed = list(prec.intercept = 1), 
         control.compute = list(cpo = TRUE), 
         ##control.inla = list(cmin = 0), 
         control.family = list(
             hyper = list(
                 prec = list(initial = 2,
                             param = c(10, 1)),
                 tail = list(initial = 0))))
