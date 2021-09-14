graph.file = "W.dat"
g = inla.read.graph(graph)
Y = rpois(g$n, exp(3 + rnorm(g$n)))
E = rep(100, g$n)
region = 1:g$n

formula = Y ~ f(region, model="bym", graph=graph, debug=T)
result = inla(formula, data = data.frame(region,Y), family = "poisson", E = E)

x = result$summary.random$region$mean
n = length(x)/2

## simulate two new datasets. 'a' is the weighting between the
## log.rel.risk:
a = 2
xx = x[1:n]
x = c(a*xx, xx/a)
E = c(E,E)
N = 2*n
y = rpois(N, lambda = E*exp(x))

## model='besag2' defines a model with length N = 2*graph->n, the
## first half is weighted with 'a' the other half is weighted with
## 1/a. here there is no unstructed terms.
i = 1:N
formula = y ~ f(i, model="besag2", graph=graph) -1
r = inla(formula, family = "poisson", data = data.frame(E,y,i), E=E, verbose=TRUE)
