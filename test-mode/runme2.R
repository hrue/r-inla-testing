INLA:::inla.my.update()
inla.setOption(inla.call = path.expand("~/bin/inla.mkl.work"))
inla.setOption(num.threads = "1:1")
n=100000
x=rnorm(n)
y=rpois(n,exp(x))

Sys.setenv(INLA_TRACE = 'integrate_func:10000')
Sys.unsetenv('INLA_TRACE')

r=inla(y ~ 1 + x + f(idx), data=data.frame(y,x, idx = 1:n),
       family="poisson", safe = F, 
       control.predictor=list(compute=TRUE),
       control.compute=list(return.marginals.predictor=TRUE),
       inla.mode="experimental",verbose=T)

if (T)for(i in 1:10) {
    inla.dev.new();
    plot(r$marginals.fitted.values[[i]],pch=19);
    abline(v=r$summary.fitted.values[i,"mode"],lwd=3,col="blue");
    abline(v=inla.mmarginal(inla.smarginal(r$marginals.fitted.values[[i]])),lwd=3,col="red")
}



