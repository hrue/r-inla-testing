if (!exists("first")) {
    INLA:::inla.my.update()
    inla.setOption(inla.call = "inla.mkl.work")
}
first <- TRUE

y <- 1
prec <- 0.1
r <- inla(y~1,
          data=data.frame(y=y),
          family = "poisson", 
          control.fixed = list(prec.intercept = prec), 
          control.inla=list(int.strategy = "eb", improved.simplified.laplace=FALSE))
rr <- inla(y~1,
          data=data.frame(y=y),
          family = "poisson", 
          verbose=TRUE,
          control.compute = list(config = TRUE), 
          control.fixed = list(prec.intercept = prec), 
          control.inla=list(int.strategy = "eb", strategy = "simplified.laplace",  improved.simplified.laplace=TRUE))
rrr <- inla(y~1,
          data=data.frame(y=y),
          family = "poisson", 
          control.fixed = list(prec.intercept = prec), 
          control.inla=list(int.strategy = "eb", strategy = "laplace"))

marg <- inla.smarginal(r$marginals.fixed[[1]])
int <- sum(marg$y * (c(diff(marg$x), 0) + c(0, diff(marg$x))/2))
marg$y <- marg$y/int
my.plot(marg, col = "blue", type = "l", lwd = 5)

marg <- inla.smarginal(rr$marginals.fixed[[1]])
int <- sum(marg$y * (c(diff(marg$x), 0) + c(0, diff(marg$x))/2))
marg$y <- marg$y/int
my.lines(marg, col = "red", lwd = 5)

marg <- inla.smarginal(rrr$marginals.fixed[[1]])
int <- sum(marg$y * (c(diff(marg$x), 0) + c(0, diff(marg$x))/2))
marg$y <- marg$y/int
my.lines(marg, col = "brown", lty = 3, lwd = 4)
