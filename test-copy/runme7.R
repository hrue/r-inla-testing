n = 1000
N = 4*n
time = 1:n
x = sqrt(2) * sin(time/n*4*pi)
age = rep(1:4, each=n)

y = numeric(N)
y[ which(age==1) ] = 0.5*x
y[ which(age==2) ] = 1.0*x
y[ which(age==3) ] = 1.5*x
y[ which(age==4) ] = 2.0*x

s <- 0.1
y = y + rnorm(N, sd = s)

formula = y ~ -1 +
    f(NAs, model = "rw2",  values = 1:n, scale.model = TRUE,
      hyper = list(prec = list(initial = 0, fixed = TRUE))) +
    f(i, copy = "NAs", hyper = list(beta = list(prior = "normal",
                                                param = c(1, 1),
                                                fixed = FALSE))) +
    f(j, copy = "NAs", hyper = list(beta = list(prior = "normal",
                                                param = c(1, 1),
                                                fixed = FALSE))) +
    f(k, copy = "NAs", hyper = list(beta = list(prior = "normal",
                                                param = c(1, 1),
                                                fixed = FALSE))) +
    f(l, copy = "NAs", hyper = list(beta = list(prior = "normal",
                                                param = c(1, 1),
                                                fixed = FALSE)))

i = rep(NA, N)
j = rep(NA, N)
k = rep(NA, N)
l = rep(NA, N)

i[which(age == 1)] = time
j[which(age == 2)] = time
k[which(age == 3)] = time
l[which(age == 4)] = time

r = inla(formula,
         data = data.frame(y, i, j, k, l, NAs = rep(NA, N)),
         control.compute = list(residuals = TRUE), 
         family = "normal",
         control.family = list(
             hyper = list(
                 prec = list(initial = log(1/s^2), fixed = TRUE))))
summary(r)
