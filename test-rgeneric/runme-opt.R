n <- 10

model = inla.rgeneric.define(inla.rgeneric.ar1.model.opt, n=n, ntheta = 2,
                             debug = TRUE, optimize = TRUE)

inla.rgeneric.q(model, "graph")
inla.rgeneric.q(model, "Q", theta = c(1, 2))

