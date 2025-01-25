slen <- 5
n <- slen * 30
## make it not perfect
eta <- sin(2*pi * 1:n/(slen+0.2))
s <- 0.5
y <- eta + rnorm(n, sd = s)
y[n:(n-2*slen)] <- NA
r <- inla(y ~ 1 + f(idx,
                    model = "seasonal",
                    scale.model=TRUE,
                    season.length = slen,
                    constr = TRUE), 
          data = data.frame(y, idx = 1:n), 
          control.family = list(hyper = list(prec = list(
                                                 initial = log(1/s^2),
                                                 fixed = TRUE))))
plot(r)

