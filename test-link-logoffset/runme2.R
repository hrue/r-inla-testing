## variant 0
a=3
b=0.5
c=2
n=100
x = sort(runif(n, min=0, max=6))
eta = a + exp(b - c*x)
sd = 0.1
y = eta + rnorm(n, sd=sd)

par(mfrow=c(1, 2))
link.x = rep(1, n)
r = inla(y ~ 1 + x,
    data = data.frame(y, x, link.x),
    family = "gaussian",
    control.family = list(list(
        hyper = list(
            prec = list(
                initial = log(1/sd^2), 
                fixed=FALSE)), 
        control.link = list(
            model = "logoffset", 
            variant = 0, 
            hyper = list(
                beta = list(
                    initial = log(a), 
                    fixed = FALSE))))), 
    control.predictor = list(compute=TRUE),
    control.inla = list(h = 0.001), 
    link.covariates = link.x)

plot(x, y, main = "variant 0")
lines(x, r$summary.fitted.value$mean, lwd=3)


## variant 1
a=3
b=0.5
c=2
n=100
x = sort(runif(n, min=0, max=6))
eta = a - exp(b - c*x) ## yes, its a minus
sd = 0.1
y = eta + rnorm(n, sd=sd)

link.x = rep(1, n)
r = inla(y ~ 1 + x,
    data = data.frame(y, x, link.x),
    family = "gaussian",
    control.family = list(list(
        hyper = list(
            prec = list(
                initial = log(1/sd^2), 
                fixed=FALSE)), 
        control.link = list(
            model = "logoffset", 
            variant = 1, ## say it here
            hyper = list(
                beta = list(
                    initial = log(a), 
                    fixed = FALSE))))), 
    control.predictor = list(compute=TRUE),
    control.inla = list(h = 0.001), 
    link.covariates = link.x)

plot(x, y, main="variant 1")
lines(x, r$summary.fitted.value$mean, lwd=3)
