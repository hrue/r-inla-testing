graphics.off()

a = seq(-15, 15, by=0.1)
lambda = 10
plot(a, inla.pc.dsn(a, lambda), type="l", lwd=3)

to.theta = inla.models()$link$sn$hyper$theta$to.theta
from.theta = inla.models()$link$sn$hyper$theta$from.theta

to.theta.diff = args(to.theta)
body(to.theta.diff) = D(body(to.theta), "x")


dev.new()
h = 0.0001
theta = to.theta(a)
dens = inla.pc.dsn(a, lambda) / abs((to.theta(a+h)-to.theta(a-h))/2/h)
plot(theta, dens, type="l")
dens = inla.pc.dsn(a, lambda) / abs(to.theta.diff(a))
lines(theta, dens, lwd=3)


