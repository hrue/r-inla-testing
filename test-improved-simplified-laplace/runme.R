if (!exists("first")) {
    INLA:::inla.my.update()
    inla.setOption(inla.call = "inla.mkl.work")
}
first <- TRUE

while(TRUE) {
    y <- rpois(4, lambda = exp(0))
    if (!all(y == 0)) break
}

prec <- 0.1
r <- inla(y~1,
          data=data.frame(y=y),
          family="poisson",
          control.fixed = list(prec.intercept = prec), 
          control.inla=list(improved.simplified.laplace=FALSE))
rr <- inla(y~1,
          data=data.frame(y=y),
          family="poisson",
          verbose=TRUE,
          control.fixed = list(prec.intercept = prec), 
          control.inla=list(improved.simplified.laplace=TRUE))
rrr <- inla(y~1,
          data=data.frame(y=y),
          family="poisson",
          control.fixed = list(prec.intercept = prec), 
          control.inla=list(strategy = "laplace"))

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

r$summary.fixed
rr$summary.fixed

xy <- r$marginals.fixed[[1]]
yy <- c()
for(x in  xy[, "x"]) {
    yy <- c(yy, prod(dpois(y, lambda = exp(x))) * dnorm(x, sd = 1/sqrt(prec)))
}
xy[, "y"] <- yy
marg <- inla.smarginal(xy)
int <- sum(marg$y * (c(diff(marg$x), 0) + c(0, diff(marg$x))/2))
marg$y <- marg$y/int
print(sum(marg$y * (c(diff(marg$x), 0) + c(0, diff(marg$x))/2)))

my.lines(marg$x, marg$y, lwd = 5, col = "black")
