set.seed(123)
n <- 1
x <- rnorm(n)
s <- 1
y <- 0 + x + rnorm(n, sd = s)
y <- 0

fix <- TRUE
INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work", num.threads = "1:1", smtp = 'band')
prec <- exp(as.numeric(inla.models()$predictor$predictor$hyper$theta$initial))

r <- inla(y ~ 1, data = data.frame(y, x), inla.mode = "classic", 
          control.fixed = list(prec.intercept = 1),
          verbose = F,
          control.inla = list(reordering = "identity"), 
          control.family = list(hyper = list(prec = list(param = c(10/s^2, 10), initial = log(1/s^2), fixed = fix))))
rr <- inla(y ~ 1, data = data.frame(y, x), inla.mode = "twostage", 
           control.fixed = list(prec.intercept = 1), 
           control.family = list(hyper = list(prec = list(param = c(10/s^2, 10), initial = log(1/s^2), fixed = fix))))
rrr <- inla(y ~ 1, data = data.frame(y, x), inla.mode = "experimental", 
            control.fixed = list(prec.intercept = 1),
            verbose = F, 
            control.family = list(hyper = list(prec = list(param = c(10/s^2, 10), initial = log(1/s^2), fixed = fix))))

print(r$mlik)
print(rr$mlik)
print(rrr$mlik)

