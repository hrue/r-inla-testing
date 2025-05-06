INLA:::inla.my.update(b = TRUE)
data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)

Germany <- rbind(Germany, Germany, Germany, Germany)
Germany <- rbind(Germany, Germany, Germany, Germany)
Germany <- rbind(Germany, Germany, Germany, Germany)

inla.setOption(num.threads = "2:4",
               smtp = 'stiles',
               verbose = !FALSE,
               safe = FALSE)

formula3 = Y ~ f(region.struct,model="besag",graph=g, scale.model = TRUE) + 
    f(region,model="iid") + f(x, model="rw2", scale.model = TRUE)

if (FALSE) r <- inla(formula3,
                     family="poisson",
                     data=Germany,
                     E=E,
                     control.inla = list(int.strategy = "eb"), 
                     control.compute = list(smtp = "taucs",
                                            control.gcpo = list(enable = TRUE,
                                                                num.level.sets = 3)))

if (!FALSE) v <- inla(formula3,
                      family="poisson",
                      data=Germany,
                      E=E,
                      control.inla = list(int.strategy = "eb"), 
                      control.compute = list(cpo = T,
                                             control.gcpo = list(enable = TRUE,
                                                                 num.level.sets = -1)))
