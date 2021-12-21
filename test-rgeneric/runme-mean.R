n <- 100

model = inla.rgeneric.define(ar1.model.opt, n=n, ntheta = 2,
                             debug = TRUE, optimize = TRUE)

inla.rgeneric.q(model, "graph")
inla.rgeneric.q(model, "Q", theta = c(1, 2))

