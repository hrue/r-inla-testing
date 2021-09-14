n = 16
g = "graph.dat"
y = rpois(n, lambda = 1)
i = paste("region", 1:n, sep="")

formula = y ~ f(i, model="bym", values = i, graph.file = g)
r = inla(formula, data = data.frame(y,i), family = "poisson")


