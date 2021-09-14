n = 1000
phi <- 0.9
nr <- 10
ng <- 1
s = 0.1
x <- arima.sim(n, model = list(ar = 0.9))
x <- x/sd(x)
y = 0 + x + rnorm(n, sd = s)

rr <- sample(1:nr, n, replace = TRUE)
gg <- sample(1:ng, n, replace = TRUE)

r1 = (inla(y ~ -1 + f(idx, model="ar1", param = c(1, 1), replicate = rr, group = gg,
                      hyper = list(prec = list(prior = "loggamma", param = c(1, 1)),
                                   rho = list(prior = "normal", param = c(0, 1)))), 
           data = data.frame(y = y, idx = 1:n, rr = rr, gg = gg),
           control.family = list(
               hyper = list(prec = list(initial=log(1/s^2), fixed=TRUE)))))

model2 = inla.rgeneric.define(inla.rgeneric.ar1.model.opt, n=n, debug=FALSE, optimize = TRUE)
r2 = (inla(y ~ -1 + f(idx, model=model2, replicate = rr, group = gg), 
           data = data.frame(y = y, idx = 1:n, rr = rr, gg = gg),
           control.family = list(
               hyper = list(prec = list(initial = log(1/s^2), fixed=TRUE)))))

model3 = inla.rgeneric.define(inla.rgeneric.ar1.model, n=n, debug=FALSE, optimize = FALSE)
r3 = (inla(y ~ -1 + f(idx, model=model3, replicate = rr, group = gg), 
           data = data.frame(y = y, idx = 1:n, rr = rr, gg = gg),
           control.family = list(
               hyper = list(prec = list(initial = log(1/s^2), fixed=TRUE)))))


print(r1$mlik - r2$mlik)
print(r1$mlik - r3$mlik)
print(r2$mlik - r3$mlik)

print(r1$cpu)
print(r2$cpu)
print(r3$cpu)
