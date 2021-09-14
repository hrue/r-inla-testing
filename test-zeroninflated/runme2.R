data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
form = Y ~ f(region,model="besag",graph=g) 
r  =  inla(form, family="zeroinflatedpoisson1", data=Germany, E=E,
           control.compute = list(config=TRUE))

ysim.fun = function(...)
{
    args = list(...)
    n = length(Predictor)
    prob = theta[1]
    zero = (rbinom(n, 1, prob = prob) == 1)
    not.zero = !zero
    n.not.zero = sum(not.zero)
    y = numeric(n)
    y[zero] = 0
    y[not.zero] = rpois(n.not.zero, lambda = args$E[not.zero] * exp(Predictor[not.zero]))
    return (y)
}
xx = inla.posterior.sample(1000, r)
yy = inla.posterior.sample.eval(ysim.fun, xx, E=Germany$E)
y.mean = apply(yy,1,mean)
germany.map(y.mean/Germany$E)
