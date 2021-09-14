data(Nile)
Nile = scale(as.numeric(Nile))
n = length(Nile)
r = inla(Nile ~ -1 + f(idx, model="fgn", order = 4),
         control.family = list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
         data = data.frame(Nile, idx = 1:n),
         verbose=TRUE)
r2 = inla(Nile ~ -1 + f(idx, model="fgn2", order = 4),
         control.family = list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
         data = data.frame(Nile, idx = 1:n),
         verbose=TRUE)
