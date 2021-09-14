n = 100
rho = 0.9
x = arima.sim(n=n, model = list(ar = rho))
x = x - mean(x)
z = rnorm(n)
y = 1 + z + x + rnorm(n, sd=0.01)

idx = 1:n
formula = y ~ 1 + z + f(idx, model="ar1", constr=TRUE)
r = inla(formula,  data = data.frame(y, z, idx),
        control.compute = list(config = TRUE))

## first sample the theta(-index)
cs = r$misc$configs
ld = numeric(cs$nconfig)
for (i in 1:cs$nconfig) {
    ld[i] = cs$config[[i]]$log.posterior
}
p = exp(ld - max(ld))
idx = sample(1:cs$nconfig, 1, prob = p)

## then the latent field
xx = cs$config[[idx]]$mean +
    inla.qsample(n=1,
                 Q=cs$config[[idx]]$Q,
                 constr = cs$constr)

a.sample = list(theta = cs$config[[idx]]$theta,  x = xx)
nm = c()
for(k in 1:length(cs$contents$tag)) {
    ii = seq(cs$contents$start[k], length = cs$contents$length[k])
    nm = c(nm,
            paste(cs$contents$tag[k],
                  ".",
                  inla.num(1:cs$contents$length[k]), sep=""))
}
rownames(a.sample$x) = nm

str(a.sample)

## if you want theta in the user-scale, like precision and not
## log-precision, you can do like this

for(k in 1:length(a.sample$theta)) {
    a.sample$theta[k] = do.call(r$misc$from.theta[[k]], args = list(a.sample$theta[k]))
}
names(a.sample$theta) = paste(names(a.sample$theta), "-- in user scale")

