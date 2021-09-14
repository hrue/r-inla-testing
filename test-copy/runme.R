n = 500
rho = 0.9
sd = 0.01
y = arima.sim(n=n, model = list(ar = rho))*sqrt(1-rho^2) + rnorm(n, sd = sd)
y[1:10]=NA  ## ``burn in''
i = 1:n
j = i
ii = c(NA, 1:(n-1))
ww = rep(-1,n)
na = rep(NA,n)

i2 = c(i,i)
ii2 = c(na, ii)
j2 = c(na, j)
ww2 = c(na, ww)

Y = matrix(NA, 2*n, 2)
Y[1:n, 1] = y
Y[1:n + n, 2] = 0

formula = Y ~ -1 + f(i2, initial=-10, fixed=T) +
    f(ii2, ww2, copy = "i2", range = c(-1,1),
      fixed = FALSE, initial = 3, precision = 1e10,
      param = c(0,0.2), prior = "normal") +
    f(j2, initial=0, fixed=FALSE)

r = inla(formula, data = data.frame(i2,ii2,ww2,Y,j2),
        family = c("gaussian","gaussian"),
        control.family = list(
                list(initial = log(1/sd^2), fixed = TRUE),
                list(initial = 13, fixed = TRUE)),
        verbose=TRUE,
        control.predictor = list(compute=TRUE))

fformula = y ~ -1 + f(i, model = "ar1")
rr = inla(fformula, data = data.frame(i,y),
        family = "gaussian",
        control.predictor = list(compute=TRUE),
        control.family = list(initial = log(1/sd^2), fixed = TRUE),
        verbose=TRUE)

##

h = inla.hyperpar(r,  diff.logdens = 10)
hh = inla.hyperpar(rr,  diff.logdens=10)

print(h$summary.hyperpar[2,"mean"])
print(hh$summary.hyperpar[2,"mean"])





