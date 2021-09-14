INLA:::inla.my.update(b = T)
##set.seed(123)
n <- 50
x <- rnorm(n, sd = 0.1)
if (FALSE) {
    eta <-  1+x
    y <- rbinom(n, size = 1, prob = 1/(1+exp(-eta)))
    family <- "binomial"
} else {
    eta <- -1+x
    y <- rpois(n, exp(eta))
    family <- "poisson"
}
par(mfrow = c(2, 2))
vb <- TRUE

rg <- inla(y ~ 1+x, data = data.frame(y, x),
          family = family, 
          control.fixed = list(prec = 0.1, prec.intercept = 0.1), 
          control.inla = list(strategy = "gaussian"), 
          control.predictor = list(compute = TRUE), 
          verbose = FALSE)
r <- inla(y ~ 1+x, data = data.frame(y, x),
          family = family, 
          control.fixed = list(prec = 0.1, prec.intercept = 0.1), 
          control.inla = list(control.vb = list(enable = vb),
                              strategy = "gaussian"), 
          control.predictor = list(compute = TRUE), 
          verbose = TRUE)
rr <- inla(y ~ 1+x, data = data.frame(y, x),
           family = family, 
           control.fixed = list(prec = 0.1, prec.intercept = 0.1), 
           control.inla = list(control.vb = list(enable = vb), 
                               strategy = "simplified.laplace"), 
           control.predictor = list(compute = TRUE), 
           keep = TRUE, 
           verbose = TRUE)
rrr <- inla(y ~ 1+x, data = data.frame(y, x),
            family = family, 
            control.fixed = list(prec = 0.1, prec.intercept = 0.1), 
            control.inla = list(control.vb = list(enable = vb), 
                                npoints = 21), 
            control.predictor = list(compute = TRUE), 
            verbose = TRUE)

print(cbind(g = rg$summary.fixed[, "mean"], 
            re = r$summary.fixed[, "mean"], 
            sla = rr$summary.fixed[, "mean"], 
            la = rrr$summary.fixed[, "mean"]))

inla.dev.new()
plot(inla.smarginal(r$marginals.fixed[[1]]), lwd = 15, col = "blue", type = "l")
lines(inla.smarginal(rg$marginals.fixed[[1]]), lwd = 7, col = "green", lty = 2)
lines(inla.smarginal(rr$marginals.fixed[[1]]), lwd = 7, col = "black", lty = 2)
lines(inla.smarginal(rrr$marginals.fixed[[1]]), lwd = 7, col = "red", lty = 3)
title("Intercept")

inla.dev.new()
plot(inla.smarginal(r$marginals.fixed[[2]]), lwd = 15, col = "blue", type = "l")
lines(inla.smarginal(rg$marginals.fixed[[2]]), lwd = 7, col = "green", lty = 2)
lines(inla.smarginal(rr$marginals.fixed[[2]]), lwd = 7, col = "black", lty = 2)
lines(inla.smarginal(rrr$marginals.fixed[[2]]), lwd = 7, col = "red", lty = 3)
title("beta.x")

