if (TRUE) {
    time = runif(100)
    z = rnorm(100)
    zz = rnorm(100)

    formula = inla.surv(time) ~ z + zz

    r = inla(formula, family = "coxph", data = data.frame(time,z,zz),
            control.hazard = list(cutpoints = c(0, 0.3,2,3), param=c(1,2)), debug = TRUE, verbose=TRUE, keep=T)
}

if (FALSE) {
    data(Leuk)
    source(system.file("demodata/Leuk-map.R", package="INLA"))
    g = system.file("demodata/Leuk.graph", package="INLA")

    formula = inla.surv(Leuk$time, Leuk$cens) ~ sex + age +
        f(inla.group(wbc), model="rw1")+
            f(inla.group(tpi), model="rw2")+
                f(district,model="besag",graph.file = g)

    r = inla(formula, family="coxph",
            data=Leuk, verbose=TRUE, 
            control.hazard=list(model="rw1", n.intervals=18),
            control.inla=list(h=0.05))

    plot(r)
}


