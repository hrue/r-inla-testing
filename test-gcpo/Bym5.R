library(INLA)

inla.setOption(scale.model = TRUE)
inla.setOption(num.threads = "6:2")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

## just make a duplicated column
Germany = cbind(Germany,region.struct=Germany$region)

if (FALSE) {
    Germany <- rbind(Germany, Germany)
    Germany <- rbind(Germany, Germany)
    Germany <- rbind(Germany, Germany)
    Germany <- rbind(Germany, Germany)
}

formula3 = Y ~ 1 + f(region.struct,model="besag",graph=g,
                     hyper = list(prec = list(prior = "pc.prec",
                                              param = c(0.5, 0.01)))) + 
    f(region,model="iid",
      hyper = list(prec = list(prior = "pc.prec",
                               param = c(0.5, 0.01)))) + 
    f(x, model="rw2",
      hyper = list(prec = list(prior = "pc.prec",
                               param = c(0.5, 0.01))))

r = inla(formula3,
         family="poisson",
         data=Germany, 
         E=E,
         verbose = !TRUE,
         control.fixed = list(prec.intercept = 1), 
         control.compute = list(
             cpo = T, 
             control.gcpo = list(
                 enable = TRUE, 
                 num.level.sets = 5,
                 size.max = 7, 
                 verbose = !TRUE)),
         safe = FALSE)

rr = inla(formula3,
          family="poisson",
          data=Germany, 
          E=E,
          verbose = !TRUE,
          control.fixed = list(prec.intercept = 1), 
          control.compute = list(
              cpo = T, 
              control.gcpo = list(
                  enable = TRUE, 
                  num.level.sets = 5,
                  size.max = 7, 
                  verbose = !TRUE)),
          inla.call = "inla.mkl.work",
          safe = FALSE,
          keep = TRUE)

if (FALSE) {
    r <- inla.rerun(r)
    rr <- inla.rerun(rr)
}

plot(r$gcpo$gcpo, rr$gcpo$gcpo); abline(a=0,b=1)

print(max(abs(r$summary.fitted.values$mean -
              rr$summary.fitted.values$mean)))
print(max(abs(r$summary.fitted.values$sd / 
              rr$summary.fitted.values$sd -1)))
