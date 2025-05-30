n = 100
rho = 0.9
x = arima.sim(n=n, model = list(ar = rho))
x = x - mean(x)
z = rnorm(n)
y = 1 + z + x + rnorm(n, sd=0.01)

idx = 1:n
formula = y ~ 1 + z + f(idx, model="ar1", constr=TRUE)
r = inla(formula,  data = data.frame(y, z, idx),
        control.compute = list(config = TRUE),
        control.inla = list(int.strategy = "grid", diff.logdens = 5, dz=.75),
        verbose = TRUE,
        keep=TRUE, 
        num.threads = 10)

cs = r$misc$configs
for (i in 1:length(cs$contents$tag)) {
    idx = seq(cs$contents$start[i],  len = cs$contents$len[i])
    mean = 0
    psum = 0
    for(k in 1:cs$nconfig) {
        mean = mean + exp(cs$config[[k]]$log.posterior) * cs$config[[k]]$mean[idx]
        psum = psum + exp(cs$config[[k]]$log.posterior)
    }
    mean = mean/psum

    print(cs$contents$tag[i])
    print(mean)
}
r$summary.fixed[,"mean"]

