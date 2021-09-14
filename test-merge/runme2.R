n = 10
y = rnorm(n)
y[1:10] = NA
x = rnorm(n)
z1 = runif(n)
z2 = runif(n)*n
idx = 1:n
idx2 = 1:n
lc1 = inla.make.lincomb(idx = c(1, 2, 3))
names(lc1) = "lc1"
lc2 = inla.make.lincomb(idx = c(0, 1, 2, 3))
names(lc2) = "lc2"
lc3 = inla.make.lincomb(idx = c(0, 0, 1, 2, 3))
names(lc3) = "lc3"
lc = c(lc1, lc2, lc3)

r = inla(y ~ 1 + x + f(idx, z1) + f(idx2, z2),
         lincomb = lc, 
         control.compute = list(config=TRUE), 
         control.fixed = list(prec.intercept = 1, prec = 1), 
         control.predictor = list(compute = TRUE, link = 1), 
         data = data.frame(y, x, idx, idx2, z1, z2))

x = rnorm(n)
rr = inla(y ~ 1 + x + f(idx, z1) + f(idx2, z2),
         lincomb = lc, 
         control.compute = list(config=TRUE), 
         control.fixed = list(prec.intercept = 1, prec = 1), 
         control.predictor = list(compute = TRUE, link = 1), 
         data = data.frame(y, x, idx, idx2, z1, z2))

x = rnorm(n)
rrr = inla(y ~ 1 + x + f(idx, z1) + f(idx2, z2),
         lincomb = lc, 
         control.compute = list(config=TRUE), 
         control.fixed = list(prec.intercept = 1, prec = 1), 
         control.predictor = list(compute = TRUE, link = 1), 
         data = data.frame(y, x, idx, idx2, z1, z2))


tref = Sys.time()
R = inla.merge(list(r, rr, rrr), prob = c(1, 2, 3), verbose=TRUE)
print(Sys.time() - tref)
