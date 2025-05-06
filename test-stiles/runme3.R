set.seed(123)
n <- 180
s <- 0.01
z <- rnorm(n)
zz <- rnorm(n)
y <- sin((1:n) / n * 4 * 2 * pi) * exp(-(1:n)/n ) + rnorm(n, sd = s)
y <- y - mean(y)

INLA:::inla.my.update(b = T)
inla.setOption(num.threads = "1:1", verbose = TRUE, safe = FALSE,  keep = !TRUE)

m <- 1
A <- matrix(rnorm(n*m), m, n)
lc <- inla.make.lincombs(idx = A)

inla.setOption(smtp = 'band')
r <- inla(y ~ 1 + z + zz + f(idx, model = "rw2", scale.model = TRUE, constr = TRUE),
          data = data.frame(y, idx = 1:n, z, zz),
          control.inla = list(int.strategy = "eb", reordering = "band"), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          lincomb = lc, 
          control.family = list(
              hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))))
inla.setOption(smtp = 'taucs')
rr <- inla(y ~ 1 + z + zz + f(idx, model = "rw2", scale.model = TRUE, constr = TRUE),
           data = data.frame(y, idx = 1:n, z, zz),
           control.inla = list(int.strategy = "eb", reordering = "metis"), 
           control.fixed = list(prec.intercept = 1, prec = 1), 
           lincomb = lc, 
           control.family = list(
               hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))))

inla.setOption(smtp = 'stiles')
rrr <- inla(y ~ 1 + z + zz + f(idx, model = "rw2", scale.model = TRUE, constr = TRUE),
            data = data.frame(y, idx = 1:n, z, zz),
            control.inla = list(int.strategy = "eb"), 
            control.compute = list(config = TRUE), 
            control.fixed = list(prec.intercept = 1, prec = 1), 
            lincomb = lc, 
            control.family = list(
                hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))))
r$mlik - rr$mlik
r$mlik - rrr$mlik

r$summary.lincomb.derived[,  c("mean", "sd")]
rr$summary.lincomb.derived[, c("mean", "sd")]
rrr$summary.lincomb.derived[, c("mean", "sd")]
