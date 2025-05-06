library(INLA)
INLA:::inla.my.update(b = T)
load("dat.RData")
head(data.frame(y,E,z,zz,Intercept_p,X1_L,X2_L, BH), 30)

r <- inla(
    inla.mdata(cbind(y, E, z, zz), cbind(Intercept_p)) ~ -1 + X1_L + X2_L +
        f(BH, model = "rw2", values = BHV,
          constr = FALSE, scale.model = TRUE,
          hyper = list(prec = list(initial = 3,
                                   fixed = !TRUE))), 
    family = "00poisson",
    data = data.frame(y,E,z,zz,Intercept_p,X1_L,X2_L, BH),
    ##control.inla = list(cmin = 0.0), 
    control.inla = list(int.strategy = "eb", cmin = 0), 
    verbose=T,
    num.threads = "4:4")
summary(r)



















