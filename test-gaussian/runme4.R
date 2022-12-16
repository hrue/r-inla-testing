N <- 100
prec <- sqrt(1:N)
y <- rnorm(N, sd = sqrt(1/prec))
r <- inla(y ~ 1, 
          scale = prec, 
          data = data.frame(y, prec),
          control.fixed = list(prec.intercept = 0), 
          control.family = list(hyper = list(
                                    prec = list(initial = log(1),
                                                fixed = TRUE))),
          verbose = TRUE,
          inla.call = "inla.mkl.work")
summary(r)
r$summary.fixed[,"sd"]
sqrt(1/sum(prec))
