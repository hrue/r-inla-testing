library(INLA)
inla.setOption(num.threads = "1:1")
inla.setOption(inla.call = "inla.mkl.work")
group.size = 10
group.each = 10
n = group.size*group.each
group.mu = rnorm(group.size)
eta = 0 + rep(group.mu,each = group.each)
s <- 0.1
y = rnorm(n = n,mean = eta,sd=s)
id = rep(1:group.size,each= group.each)
groups = rep(list(list()),n)
for(i in 1:n){
  groups[[i]] = which(id == id[i])
}

family = "gaussian"
vb = FALSE

formula = y ~ -1 + f(id,model = "iid",hyper = list(prec = list(initial = 0, fixed = T, prior =  "pc.prec",param = c(0.5, 0.01))))
res = inla(formula,
           family=family,
           control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))), 
           data=list(y=y,id=id),
           inla.mode = "experimental",
           control.compute = list(cpo = T, control.gcpo = list(enable = T,num.level.sets =3,verbose = T),config = T),
           control.inla = list(int.strategy = "eb"),
           keep = T, 
           verbose = T)

res.recombine = numeric(n)
for(idx in 1:n) {
    mu.i = res$gcpo$mean[idx]
    sd.i = res$gcpo$sd[idx]
    sd.y <- s
    res.recombine[idx] = dnorm(x = y[idx], mean = mu.i, sd = sqrt(sd.i^2+sd.y^2))
    print(round(dig = 4, c(idx = idx,
                           mean = mu.i,
                           sd = sd.i,
                           gcpo = res.recombine[idx],
                           inla.gcpo = res$gcpo$gcpo[idx])))
}


my.plot(res$gcpo$gcpo,res.recombine)
abline(a = 0, b = 1)


if (FALSE) {
    library(statmod)
    gq <- gauss.quad.prob(13, dist="normal")
    yy <- 0
    sd <- 0.1
    print(c(sum(dnorm(yy, mean = gq$nodes, sd = sd) * gq$weights),
            dnorm(yy, mean = 0, sd = sqrt(1 + s^2))))

    cc <- 100
    sigma = 1/sqrt(cc)
    gq <- gauss.quad.prob(13, dist="normal", sigma = sigma)
    yy <- 0
    sd <- 0.1
    print(c(sum(dnorm(yy, mean = gq$nodes, sd = sd) *
                gq$weights * dnorm(gq$nodes, sd = 1) / dnorm(gq$nodes, sd = sigma)), 
            dnorm(yy, mean = 0, sd = sqrt(1 + s^2))))
}
