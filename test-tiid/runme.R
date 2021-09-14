n = 1
i = 1
y = rep(0, n)
dof = 5
prec= 1

## this is to extract the to.theta functions...
prop = inla.models()$likelihood$t$hyper

formula = (y ~ -1 + f(i, model="iid",
                      hyper = list(prec=list(initial=-10, fixed=TRUE))))
r = inla(
        formula,
        data = data.frame(y, i),
        family = "t",
        control.data = list(
                hyper=list(
                        prec = list(
                                initial = prop$theta1$to.theta(prec), 
                                fixed=TRUE
                                ), 
                        dof = list(
                                initial= prop$theta2$to.theta(dof), 
                                fixed=TRUE
                                )
                        )
                ),
        control.inla = list(
                ##strategy = "simplified.laplace"
                strategy = "laplace", npoints=21
                ##strategy = "gaussian"
                )
        )

m = r$marginals.random$i[[1]]
plot(m[, "x"],  log(m[, "y"]))
                                 
f = sqrt(dof/(dof-2))
x = seq(-6, 6, len=1000)
lines(x, dt(x*f, df=dof, log=TRUE)+log(f))
