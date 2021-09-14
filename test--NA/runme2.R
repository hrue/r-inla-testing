nrow = 5
ncol = 5
n = ncol * nrow
nd = round((nrow*ncol)/2)
i = sample(1:n, nd, replace=F)
i2 = inla.node2lattice(i,nrow,ncol)
stdev = 0.0001
y = i2$irow + i2$icol

formula = y ~ f(i, model="rw2d", nrow = nrow, ncol=ncol, initial = 7, fixed=T, constr=F, diagonal=0)-1
r = inla(formula, data = data.frame(y,i), verbose=T,
        control.data = list(initial = log(1/sqrt(stdev)), fixed=T),
        control.predictor = list(compute=T),
        keep=T)

A = matrix(NA, nrow, ncol)
for (ii in 1:length(i))
    A[i2$irow[ii], i2$icol[ii]] = y[ii]

round(A)
round(matrix(r$summary.random$i$mean,nrow,ncol))

j = setdiff(1:n, i)
ii = c(i,j)
yy = c(y, rep(NA,length(j)))
formula = yy ~ f(ii, model="rw2d", nrow=nrow, ncol=ncol, initial = 7, fixed=T, constr=F, diagonal=0)-1
rr = inla(formula, data = data.frame(yy,ii), verbose=F,
        control.data = list(initial = log(1/sqrt(stdev)), fixed=T),
        control.predictor = list(compute=T),
        keep=T)

iii = numeric(n)
iii[] = 1:n
iii[i] = i
yyy = numeric(n)
yyy[] = NA
yyy[i] = y
formula = yyy ~ f(iii, model="rw2d", nrow=nrow, ncol=ncol, initial = 7, constr=F, fixed=T, diagonal=0)-1
rrr = inla(formula, data = data.frame(yyy,iii), verbose=F,
        control.data = list(initial = log(1/sqrt(stdev)), fixed=T),
        control.predictor = list(compute=T),
        keep=T)

print(round(cbind(r$summary.random$i$mean,
                  rr$summary.random$ii$mean,
                  rrr$summary.random$iii$mean)))

