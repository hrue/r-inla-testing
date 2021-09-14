data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))

Germany$x <- inla.group(Germany$x)
## just make a duplicated column

formula3 = Y ~ 1 + f(region, model = "besag", vb.correct = T,
                     scale.model = TRUE, graph = g, 
                     hyper =  list(prec = list(
                                       initial = log(40), 
                                       fixed = TRUE,
                                       prior = "pc.prec",
                                       param = c(.1, 0.01)))) +
    f(x, model = "rw2", scale.model = TRUE, vb.correct = T)
    


inla.setOption(smtp = 'taucs')
inla.setOption(num.threads = 1)

r1 = inla(formula3,
          family="poisson",
          data=Germany,
          E=E,
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.predictor = list(compute = TRUE,
                                   hyper =  list(prec =  list(
                                                     initial = 20,
                                                     fixed = TRUE))), 
          verbose = T, 
          control.inla = list(
              strategy = "gaussian", 
              control.vb = list(enable = FALSE)))

r1e = inla(formula3,
           family="poisson",
           data=Germany,
           E=E,
           control.fixed = list(prec.intercept = 1, prec = 1), 
           control.predictor = list(compute = TRUE,
                                    hyper =  list(prec =  list(
                                                      initial = 20,
                                                      fixed = TRUE))), 
           verbose = T, 
           control.inla = list(
               strategy = "gaussian", 
               control.vb = list(enable = TRUE)))

r2 = inla(formula3,family="poisson",data=Germany,E=E,
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.predictor = list(compute = TRUE,
                                   hyper =  list(prec =  list(
                                                     initial = 20,
                                                     fixed = TRUE))), 
          verbose = T, 
          control.inla = list(
              control.vb = list(enable = FALSE)))

r2e = inla(formula3,family="poisson",data=Germany,E=E,
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.predictor = list(compute = TRUE,
                                   hyper =  list(prec =  list(
                                                     initial = 20,
                                                     fixed = TRUE))), 
          verbose = T, 
          control.inla = list(
              control.vb = list(enable = TRUE)))

r3 = inla(formula3,family="poisson",data=Germany,E=E,
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.predictor = list(compute = TRUE,
                                   hyper =  list(prec =  list(
                                                     initial = 20,
                                                     fixed = TRUE))), 
          verbose = T, 
          control.inla = list(
              strategy = "laplace", 
              control.vb = list(enable = FALSE)))

r3e = inla(formula3,family="poisson",data=Germany, E = E,
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.predictor = list(compute = TRUE,
                                   hyper =  list(prec =  list(
                                                     initial = 20,
                                                     fixed = TRUE))), 
          verbose = T, 
          control.inla = list(
              strategy = "laplace", 
              control.vb = list(enable = TRUE)))

inla.dev.new()

plot(inla.smarginal(r1e$marginals.fixed[[1]]), type="l", lwd=3, col = "blue")
points((r2$marginals.fixed[[1]]), col = "blue")
lines(inla.smarginal(r2e$marginals.fixed[[1]]), type="l", lwd=3, col = "blue")
points((r3$marginals.fixed[[1]]), col = "blue")
lines(inla.smarginal(r3e$marginals.fixed[[1]]), type="l", lwd=3, col = "blue")


print(round(inla.rbind.data.frames(r1$summary.fixed, r1e$summary.fixed,
                                   r2$summary.fixed, r2e$summary.fixed,
                                   r3$summary.fixed, r3e$summary.fixed)[, c("mean", "sd")], 
            dig = 3))
