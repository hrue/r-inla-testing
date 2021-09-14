## if (!exists("DONE")) {
##     INLA:::inla.my.update()
## }
## DONE <- 1
## inla.setOption(inla.call="inla.mkl.work")

n <- 300
x <- rnorm(n, sd = 0.3)
df <- 1
prec <- 3
eta <- 1 + x
lambda <- exp(eta)
y <- sqrt(rchisq(n, df = df, ncp = prec * lambda^2) /prec)

r <- inla(y ~ 1 + x, 
          data = data.frame(y, x),
          family = "fmri",
          control.family = list(hyper = list(df = list(initial = df))), 
          control.inla = list(cmin = 0,
                              int.strategy = "eb",
                              strategy = "adaptive"), 
          control.compute = list(cpo = TRUE), 
          verbose = TRUE)
## Seems not needed
r$.args$control.inla$cmin <- -Inf
r$.args$control.inla$int.strategy <- "auto"
rr <- inla.rerun(r)
summary(rr)


## cencoring. if y< val, then its value is ignored, but
## the information that its < val, is used.

val <- quantile(y, 0.15) ## just to have a reasonable value
to.small <- (y < val)
event <- numeric(n)
truncation <- numeric(n)
time2 <- rep(NA, n) ## not in use

## left censored, know its smaller than 'val' only.
## may also chose to remove these data from the
## dataset
y[to.small] <- val 
event[to.small] <- 2 

## observations from the truncated distribution y|y>val
event[!to.small] <- 1 
truncation[!to.small] <- val
Y <- inla.surv(y, event, time2, truncation)

rrr <- inla(Y ~ 1 + x, 
          data = list(Y = Y, x = x),
          family = "fmrisurv",
          control.family = list(hyper = list(df = list(initial = df))), 
          control.inla = list(cmin = 0,
                              int.strategy = "eb",
                              strategy = "adaptive"), 
          control.compute = list(cpo = TRUE), 
          verbose = TRUE)
## Seems not needed
rrr$.args$control.inla$cmin <- -Inf
rrr$.args$control.inla$int.strategy <- "auto"
r4 <- inla.rerun(rrr)

summary(r4)
