
n = 10
y = rpois(n, 1)

formula = y ~ 1 + x + xx + f(idx) + f(iidx)

x = rnorm(n, sd = 0.2)
xx = rnorm(n, sd = 0.2)
idx = rep(1:2, each = n %/% 2)
iidx = 1:n

r = inla(formula,
         data = data.frame(y, x, xx, idx, iidx),
         family = "poisson",
         verbose = TRUE,
         control.inla = list(strategy = "adaptive", adaptive.max = 5))

r.def = inla(formula,
             data = data.frame(y, x, xx, idx, iidx),
             family = "poisson")

r.g = inla(formula,
           data = data.frame(y, x, xx, idx, iidx),
           family = "poisson",
           control.inla = list(strategy = "gaussian"))

round(cbind(adaptive=r$summary.fixed[,"mean"],
            default=r.def$summary.fixed[,"mean"],
            gaussian=r.g$summary.fixed[,"mean"]), digits = 6)

round(cbind(adaptive=r$summary.random$idx$mean, 
            default=r.def$summary.random$idx$mean, 
            gaussian=r.g$summary.random$idx$mean), digits = 6)

round(cbind(adaptive=r$summary.random$iidx$mean, 
            default=r.def$summary.random$iidx$mean, 
            gaussian=r.g$summary.random$iidx$mean), digits = 6)


