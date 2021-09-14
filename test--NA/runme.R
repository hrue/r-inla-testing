n = 10
nd = 5
i = sample(1:n, nd, replace=F)
stdev = 0.000001
y = i

formula = y ~ f(i, model="rw2", values=1:n, initial = 4, fixed=F) 
r = inla(formula, data = data.frame(y,i), verbose=F,
        control.family = list(initial = log(1/sqrt(stdev)), fixed=T),
        control.predictor = list(compute=T),
        keep=T)


j = setdiff(1:n, i)
ii = c(i,j)
yy = c(y, rep(NA,length(j)))
formula = yy ~ f(ii, model="rw2", values=1:n, initial = 4, fixed=F)
rr = inla(formula, data = data.frame(yy,ii), verbose=F,
        control.family = list(initial = log(1/sqrt(stdev)), fixed=T),
        control.predictor = list(compute=T),
        keep=T)

iii = numeric(n)
iii[] = 1:n
iii[i] = i
yyy = numeric(n)
yyy[] = NA
yyy[i] = y
formula = yyy ~ f(iii, model="rw2", values=1:n, initial = 4, fixed=F) 
rrr = inla(formula, data = data.frame(yyy,iii), verbose=F,
        control.family = list(initial = log(1/sqrt(stdev)), fixed=T),
        control.predictor = list(compute=T),
        keep=T)

print(round(cbind(r$summary.random$i$mean + r$summary.fixed[1,1],
                  rr$summary.random$ii$mean + rr$summary.fixed[1,1],
                  rrr$summary.random$iii$mean + rrr$summary.fixed[1,1])))
