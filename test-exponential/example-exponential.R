n = 1000
x = rnorm(n, sd = 0.5)
lambda = exp(1+x)
yy = rexp(n, rate=lambda)
ys <-  rexp(n, rate = exp(1))
y <- pmin(yy, ys)
event <- as.numeric(ys > yy)

data = list(y=y, event=event, x=x)
summary(inla(inla.surv(y,event) ~ x,
             family ="exponential.surv",
             control.family = list(link = list(model = "neglog")), 
             data=data))

library(survival)
summary(survreg(Surv(y, event) ~ x, data=data, dist="exponential"))
