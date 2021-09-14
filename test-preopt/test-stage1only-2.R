n <- 10
x <- rnorm(n, sd = 0.2)
eta <- -1 + x
y <- rpois(n, exp(eta))

inla.setOption(inla.call = "inla.valgrind")
inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(num.threads = "2:2")
lc1 <- inla.make.lincomb("(Intercept)" = 1, x = 1)
names(lc1) <- "lc1"
lc2 <- inla.make.lincomb("(Intercept)" = 1, x = -1)
names(lc2) <- "lc2"

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson",
          lincomb = c(lc1, lc2), 
          control.inla = list(control.twostage = list(stage1only = TRUE),
                              control.vb = list(enable = TRUE)), 
          control.fixed = list(prec = 1, prec.intercept = 1), 
          control.compute = list(cpo = T, dic = T), 
          twostage = TRUE, 
          verbose = TRUE,
          keep = T)

rr <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson",
          lincomb = c(lc1, lc2), 
          control.predictor = list(compute = TRUE), 
          control.inla = list(control.twostage = list(stage1only = TRUE),
                              control.vb = list(enable = TRUE)), 
          control.fixed = list(prec = 1, prec.intercept = 1), 
          control.compute = list(cpo = T, dic = T))

a <- r$summary.fixed[,"mean"]
intercept <- a[1]
beta <- a[2]
lp <- intercept + beta * x
print(round(dig = 4, cbind(manual = lp,
                           r = r$summary.linear.predictor[, "mean"],
                           rr = rr$summary.linear.predictor[, "mean"],
                           r.mode = r$mode$x[1:length(lp)],
                           rr.mode = rr$mode$x[1:length(lp)])))
