data(Nile)
Nile = as.numeric(Nile)
Nile = Nile - mean(Nile)

n = length(Nile)
r = inla(Nile ~ 1 + f(idx, model="fgn", constr=TRUE),
         control.family = list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
         data = data.frame(Nile, idx = 1:n),
         verbose=TRUE)

par(mfrow=c(2, 2))
plot(Nile, type="l",  lwd=2,  main = "Nile data, centered", ylim = range(Nile))
plot(r$summary.random$idx$mean[n + 1:n], type="l", lwd=2, main = "First component", ylim = range(Nile))
plot(r$summary.random$idx$mean[2*n + 1:n], type="l", lwd=2, main = "Second component", ylim = range(Nile))
plot(r$summary.random$idx$mean[3*n + 1:n], type="l", lwd=2, main = "Third component", ylim = range(Nile))
