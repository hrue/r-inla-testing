library(MASS)
data(Germany)
g = inla.read.graph(system.file("demodata/germany.graph", package="INLA"))
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)
Q = -inla.graph2matrix(g)
diag(Q) = g$nnbs
Q = Q * exp(mean(log(diag(ginv(as.matrix(Q))))))

tau = 1
rho = 0.3
n = dim(Q)[1]
u = inla.qsample(n = 1,  Q = Q + 1e-8 * Diagonal(n),
        constr=list(A = matrix(1, 1, n), e=0))
v = rnorm(n)
x = 1/sqrt(tau) * (sqrt(1-rho) * v + sqrt(rho) * u)
intercept = log(Germany$E)
y = rpois(n, lambda = exp(intercept + x))

formula = y ~ 1 + f(region,
        model = "bym2", graph = g, constr=TRUE)
r = inla(formula, family="poisson",
        data=data.frame(y, region=Germany$region), keep=T)

par(mfrow=c(1, 2))
germany.map(r$summary.random$region$mean[1:n]-x)
plot(r$summary.random$region$mean[1:n],x)
abline(a=0, b=1)
