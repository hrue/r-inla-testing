n = 1000
x = arima.sim(n=n,  model = list(ar = 0.9)) + 3*sin((1:n)/n * 2*pi)
s = 0.01

y = numeric(n)
y.loc = numeric(n)

k = 1
kk = 1

ii = c()
jj = c()
values = c()

for(j in 1:n) {

    if (k+j > n)
        break
    
    y[kk] = mean(x[k:(k+j)]) + rnorm(1, sd=s)
    y.loc[kk] = mean(k:(k+j))

    val.add = rep(1/(length(k:(k+j))), length(k:(k+j)))
    ii = c(ii,  rep(kk,  length(val.add)))
    jj = c(jj, k:(k+j))
    values = c(values,  val.add)
            
    k = k+j+1
    kk = kk + 1
}
kk = kk -1

plot(x)
y = y[1:kk]
y.loc = y.loc[1:kk]

A  = sparseMatrix(i=ii,  j = jj, x = values)

i = 1:dim(A)[2]
ii = i

formula = y ~ f(i,  model="ar1",  initial=c(4, 3),  values = 1:n)  + 
              f(ii,  model="rw2",  initial=c(10), param =c(1,  0.001), values=1:n, constr=T) 

r = inla(formula, 
        data = list(i=i,  y=y, ii=ii),
        control.family = list(initial = log(1/s^2),  fixed=TRUE, param=c(1, 1)), 
        control.predictor = list(A=A, compute=TRUE),
        control.compute = list(q=TRUE, config=TRUE), 
        verbose=TRUE)

m = (r$summary.random$i$mean + r$summary.random$ii$mean)[i]
mlow = (r$summary.random$i$"0.025quant" + r$summary.random$ii$"0.025quant")[i]
mhigh = (r$summary.random$i$"0.975quant" + r$summary.random$ii$"0.975quant")[i]

    
plot(y.loc,  y,  col="blue",  ylim = c(-10, 10))
lines(i, m,  col="red",  lwd =3)
lines(i, mlow,  col = "blue",  lwd=2)
lines(i, mhigh,  col = "blue",  lwd=2)
