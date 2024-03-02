## demo on 'replicate' and how to extract its values

n = 50
nrep = 4
xvalues = 1:n
curve = sin(xvalues/2)/(xvalues/2)

y1 =  curve + rnorm(n, sd = 0.1)
y2 = -curve + rnorm(n, sd = 0.1)
y3 =  rev(curve) + rnorm(n, sd = 0.1)
y4 = -rev(curve) + rnorm(n, sd = 0.1)

y = c(y1, y2, y3, y4)
idx = rep(xvalues,  nrep)
rep = rep(1:nrep,  each = n)

r = inla(y ~ 1 + f(idx, model="rw2", replicate = rep),
    data = data.frame(y, idx, rep), verbose = T)

rr = inla(y ~ 1 + f(idx, model="rw2", replicate = rep),
    data = data.frame(y, idx, rep), verbose = T, inla.call = "inla.mkl.work")


par(mfrow=c(2, 2))

plot(xvalues, r$summary.random$idx$mean[1:n],  type = "l", lwd=2)
points(xvalues,  y1)

plot(xvalues, r$summary.random$idx$mean[n + 1:n],  type = "l", lwd=2)
points(xvalues,  y2)

plot(xvalues, r$summary.random$idx$mean[2*n + 1:n],  type = "l", lwd=2)
points(xvalues,  y3)

plot(xvalues, r$summary.random$idx$mean[3*n + 1:n],  type = "l", lwd=2)
points(xvalues,  y4)
