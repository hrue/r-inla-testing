n <- 30
y <- rpois(n, exp(2))
idx <- 1:n

## first we force the vb-correction to fail
emerg <- 0.000001
r <- inla(y ~ 1 + f(idx, vb.correct = T, initial = log(1), fixed = F), 
          data = data.frame(y, idx),
          control.inla = list(control.vb = list(emergency = emerg), 
                              int.strategy = "eb"), 
          family = "poisson",
          control.compute = list(config = TRUE))

## we can check this computing 'all.good':

## all.good==TRUE means that vb-correction is in effect for all configurations

## all.good==FALSE means that vb-correction is 0, for at least one configuration, which happens
## either in the Gaussian case, or the vb-correction aborted due to suspecious large values

all.good <- TRUE
for(i in 1:r$misc$configs$nconfig) {
    all.good <-  all.good &
        !all(r$misc$configs$config[[i]]$mean == r$misc$configs$config[[i]]$improved.mean)
}
print(all.good)

emerg <- 25 ## default value
r <- inla(y ~ 1 + f(idx, vb.correct = T, initial = log(1), fixed = T), 
          data = data.frame(y, idx),
          control.inla = list(control.vb = list(emergency = emerg), 
                              int.strategy = "eb"), 
          family = "poisson",
          control.compute = list(config = TRUE))

all.good <- TRUE
for(i in 1:r$misc$configs$nconfig) {
    all.good <-  all.good &
        !all(r$misc$configs$config[[i]]$mean == r$misc$configs$config[[i]]$improved.mean)
}
print(all.good)
