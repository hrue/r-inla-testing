n = 100
x = runif(n)
eta = 1 + x
lambda = exp(eta)
y = rpois(n, lambda = lambda)

for(step.len in c(1e-3, 1e-4, 1e-5, 1e-6, 1e-7)) {
    for(stencil in c(3, 5, 7)) {

        r = inla(y ~ 1 + x, data = data.frame(y, x),
                family = "nbinomial", 
                control.inla = list(
                        int.strategy = "grid",
                        diff.logdens = 10,
                        dz = 0.5, 
                        step.len = step.len,
                        stencil = stencil))

        print(round(c(log10(step.len), stencil, r$mlik[1]), 6))

    }
}

