n <- 30
x <- rnorm(n)
y <- 1 + x + rnorm(n)

INLA:::inla.my.update(b = T)
inla.setOption(num.threads = "1:1", safe = FALSE, inla.mode = "compact")

r <- inla(y ~ 1 + x,
          data = data.frame(y, x))

rFF <- inla(y ~ 1 + x,
            data = data.frame(y, x),
            control.mode = list(result = r, restart = F, fixed = F))
rTF <- inla(y ~ 1 + x,
            data = data.frame(y, x),
            control.mode = list(result = r, restart = T, fixed = F))
rFT <- inla(y ~ 1 + x,
            data = data.frame(y, x),
            control.mode = list(result = r, restart = F, fixed = T))
rTT <- inla(y ~ 1 + x,
            data = data.frame(y, x),
            control.mode = list(result = r, restart = T, fixed = T))

rr <- inla(y ~ 1 + x,
           data = data.frame(y, x),
           family = "stdgaussian", 
           control.mode =  list(theta = numeric(0), 
                                restart = T),
           control.inla = list(verbose = T),
           control.fixed = list(prec = 0), 
           verbose = T)
rrr <- inla(y ~ -1 + intercept + beta, 
            data = list(y = y, x = x, intercept = c(1, 0), beta = c(0, 1), A = cbind(1, x)),
           family = "stdgaussian", 
           control.mode =  list(theta = numeric(0), 
                                restart = T),
           control.inla = list(verbose = T),
           control.predictor = list(A = A), 
           control.fixed = list(prec = 0), 
           verbose = T)
