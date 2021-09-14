n = 100
phi = 0.9
x = as.numeric(arima.sim(n=n,  model = list(ar = phi)))
prec.x = 1 - phi^2
eta = x 

h = 15L
nh = 2L*h + 1L
kern = dnorm(-h:h, sd= h/3)
kern = kern / sum(kern)

A = toeplitz(c(kern[-(1:h)],  rep(0,  n-nh),  kern[1:h]))
y = rpois(n, lambda = exp(A %*% eta))

i = 1:n
formula = y ~ f(i,  model="ar1") -1
r = inla(formula, control.predictor = list(A = A),
        data = data.frame(y, i), family = "poisson")


dev.new()
plot(i, y, pch=19,  ylim = range(pretty(x)))
dev.print(postscript, "1.eps")

dev.new()
plot(i, x, col="blue",  lwd=2,  type="l",  ylim = range(pretty(x)))
dev.print(postscript, "2.eps")

dev.new()
plot(i, x, col="blue",  lwd=2,  type="l",  ylim = range(pretty(x)))
polygon(h+1 + (-h:h), kern/max(kern)+min(x),  col="gray")
dev.print(postscript, "3.eps")

dev.new()
plot(i, x, col="blue",  lwd=2,  type="l",  ylim = range(pretty(x)))
lines(i, A%*%eta,  lwd=2,  lty=2)
polygon(h+1 + (-h:h), kern/max(kern)+min(x),  col="gray")
dev.print(postscript, "4.eps")

dev.new()
plot(i, x, col="blue",  lwd=2,  type="l",  ylim = range(pretty(x)))
lines(i, A%*%eta,  lwd=2,  lty=2)
polygon(h+1 + (-h:h), kern/max(kern)+min(x),  col="gray")
points(i, y, pch=19)
dev.print(postscript, "5.eps")

dev.new()
plot(i, x, col="blue",  lwd=2,  type="l",  ylim = range(pretty(x)))
points(y, pch=19)
lines(i, A%*%eta,  lwd=2,  lty=2)
lines(i, r$summary.random$i$mean,  lwd=2)
polygon(h+1 + (-h:h), kern/max(kern)+min(x),  col="gray")
dev.print(postscript, "6.eps")

dev.new()
m = r$marginals.hyperpar[[2]]
plot(inla.spline(m),  type="l",  lwd=2)
lines(c(phi, phi),  c(0, 1000), col="red", lwd=2)
title("Lag one correlation")
dev.print(postscript, "7.eps")
