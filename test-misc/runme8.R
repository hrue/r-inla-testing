n <- 2
y <- 1:n
off <- rep(2, n)

inla.setOption(inla.call = "inla.mkl.work")

r <- inla(y ~ 1,
          data = data.frame(y),
          family = "nbinomial",
          control.inla = list(control.vb = list(enable = FALSE)))

r1 <- inla(y ~ 1 + offset(off), 
          data = data.frame(y, off),
          family = "nbinomial",
          control.inla = list(control.vb = list(enable = FALSE)))

c(r$summary.linear.predictor$mean[1], r$summary.linear.predictor$sd[1])
c(r1$summary.linear.predictor$mean[1], r1$summary.linear.predictor$sd[1])

