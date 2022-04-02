inla.setOption(smtp = "taucs")
n <- 100
x <- rnorm(n)
idx <- 1:n
eta <- 1 + x
Am <- abs(INLA:::inla.rw1(n))
Am <- matrix(rnorm(n), n, n)
##Am <- diag(n)
s <- 0.1
y <- as.numeric(Am %*% eta) + rnorm(n, sd = s)
make.plot <- FALSE
r <- inla(y ~ 1 + x, 
          data = list(y = y, x = x, idx = idx),
          control.predictor = list(A = Am, compute = T), 
          ##control.predictor = list(compute = T), 
          control.compute = list(po = TRUE, cpo = TRUE, waic = TRUE, dic = TRUE, return.marginals.predictor = TRUE), 
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = !TRUE))), 
          control.fixed = list(prec.intercept = 1), 
          control.inla = list(int.strategy = 'ccd'), 
          inla.call = "inla.mkl",
          num.threads = "1:1", 
          inla.mode = "classic")
rr <- inla(y ~ 1 + x, 
           data = list(y = y, x = x, idx = idx),
           control.predictor = list(A = Am, compute = T), 
           ##control.predictor = list(compute = T), 
           control.compute = list(po = TRUE, cpo = TRUE, waic = TRUE, dic = TRUE, return.marginals.predictor = TRUE), 
           control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = !TRUE))), 
           control.fixed = list(prec.intercept = 1), 
           control.inla = list(int.strategy = 'ccd'), 
           inla.call = "inla.mkl.work",
           num.threads = "1:1", 
           inla.mode = "experimental")

print(r$mlik - rr$mlik )
print(mean(abs(r$summary.linear.predictor$mean - rr$summary.linear.predictor$mean)))
print(mean(abs(r$summary.linear.predictor$sd / rr$summary.linear.predictor$sd -1)))
print(mean(abs(r$summary.fitted.values$mean - rr$summary.fitted.values$mean)))
print(mean(abs(r$cpo$cpo - rr$cpo$cpo)))
print(mean(abs(r$waic$waic - rr$waic$waic)))
print(mean(abs(r$dic$dic - rr$dic$dic)))
print(mean(abs(r$po$po - rr$po$po)))

plot(r$po$po,  rr$po$po);abline(a = 0, b = 1)

if (make.plot) for(i in 1:length(r$marginals.linear.predictor)) {
    plot(inla.smarginal(r$marginals.linear.predictor[[i]]), type = "l")
    lines(inla.smarginal(rr$marginals.linear.predictor[[i]]), type = "l", lwd = 2)
    title(paste0("With A: idx= ",  i))
    inla.dev.new()
}

y <- eta + rnorm(n, sd = s)
r <- inla(y ~ 1 + x, 
          data = data.frame(y = y, x = x, idx = idx),
          control.predictor = list(compute = T), 
          control.compute = list(po = TRUE, cpo = TRUE, waic = TRUE, dic = TRUE, return.marginals.predictor = TRUE), 
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))), 
          inla.call = "inla.mkl",
          num.threads = "1:1", 
          inla.mode = "classic")
r <- inla.rerun(r)
rr <- inla(y ~ 1 + x, 
          data = data.frame(y = y, x = x, idx = idx),
          control.predictor = list(compute = T), 
          control.compute = list(po = TRUE, cpo = TRUE, waic = TRUE, dic = TRUE, return.marginals.predictor = TRUE), 
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))), 
          inla.call = "inla.mkl.work",
          num.threads = "1:1", 
          inla.mode = "experimental")
rr <- inla.rerun(rr)

print(r$mlik - rr$mlik )
print(mean(abs(r$summary.linear.predictor$mean - rr$summary.linear.predictor$mean)))
print(mean(abs(r$summary.fitted.values$mean - rr$summary.fitted.values$mean)))
print(mean(abs(r$cpo$cpo - rr$cpo$cpo)))
print(mean(abs(r$waic$waic - rr$waic$waic)))
print(mean(abs(r$dic$dic - rr$dic$dic)))
print(mean(abs(r$po$po - rr$po$po)))

if (make.plot) for(i in 1:length(r$marginals.linear.predictor)) {
    plot(inla.smarginal(r$marginals.linear.predictor[[i]]), type = "l")
    lines(inla.smarginal(rr$marginals.linear.predictor[[i]]), type = "l", lwd = 2)
    title(paste0("With A: idx= ",  i))
    inla.dev.new()
}

