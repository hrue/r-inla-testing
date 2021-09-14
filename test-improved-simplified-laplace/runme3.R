if (!exists("first")) {
    INLA:::inla.my.update()
    inla.setOption(inla.call = "inla.mkl.work")
}
first <- TRUE

y <- rpois(5, lambda = exp(-1))
prec <- 0.1
r <- inla(y~1,
          data=data.frame(y=y),
          family = "poisson", 
          control.fixed = list(prec.intercept = prec), 
          control.compute = list(config = TRUE), 
          control.inla=list(int.strategy = "eb", strategy = "simplified.laplace",  improved.simplified.laplace=FALSE))
rr <- inla(y~1,
          data=data.frame(y=y),
          family = "poisson", 
          verbose=TRUE,
          control.compute = list(config = TRUE), 
          control.fixed = list(prec.intercept = prec), 
          control.inla=list(int.strategy = "eb", strategy = "simplified.laplace", improved.simplified.laplace=TRUE))

r$misc$configs$config[[1]]
rr$misc$configs$config[[1]]
