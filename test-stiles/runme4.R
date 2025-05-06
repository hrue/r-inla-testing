set.seed(123)
n <- 200
s <- 0.01
z <- rnorm(n)
zz <- rnorm(n)
zzz <- rnorm(n)

y <- sin((1:n) / n * 4 * 2 * pi) * exp(-(1:n)/n ) + rnorm(n, sd = s)
y <- y - mean(y)

m <- 10
A <- matrix(rnorm(n*m), m, n)
lc <- inla.make.lincombs(idx = A)

inla.setOption(num.threads = "4:1")

inla.setOption(smtp = 'taucs')
r <- inla(y ~ 1 + z + zz + zzz +
              f(idx, model = "rw2", scale.model = TRUE, constr = TRUE,
                hyper = list(prec = list(initial = 3, fixed = !TRUE))), 
          data = data.frame(y, idx = 1:n, z, zz, zzz),
          lincomb = lc, 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.compute = list(return.marginals.predictor = TRUE), 
          control.inla = list(control.vb = list(enable = FALSE)), 
          control.family = list(
              hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))))

inla.setOption(smtp = 'stiles')
rr <- inla(y ~ 1 + z + zz + zzz +
               f(idx, model = "rw2", scale.model = TRUE, constr = TRUE,
                 hyper = list(prec = list(initial = 3, fixed = !TRUE))), 
           data = data.frame(y, idx = 1:n, z, zz, zzz),
           lincomb = lc, 
           control.fixed = list(prec.intercept = 1, prec = 1), 
           control.compute = list(return.marginals.predictor = TRUE), 
           control.inla = list(control.vb = list(enable = FALSE)), 
           control.family = list(
               hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))))

max(abs(r$mlik - rr$mlik))
max(abs(r$summary.random$idx -rr$summary.random$idx))
max(abs(r$summary.linear.predictor -rr$summary.linear.predictor))
max(abs(r$summary.lincomb.derived -rr$summary.lincomb.derived))
