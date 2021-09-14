##inla.my.update()
##inla.setOption("inla.call", "inla")
library(mvtnorm)

ng=5
nrep=5
n = 1000
N = ng*n*nrep
y = 1:N
rho = 0.5
sd.rw = 0.05
order = 2

z = matrix(0, n, ng*nrep)
cov.mat = matrix(rho,ng,ng)
diag(cov.mat) = 1
for(i in 1:n)
{
    for(r in 1:nrep)
    {
        x = rmvnorm(1, sigma = sd.rw^2 * cov.mat)
        j = inla.idx(1, n=1, group=1:ng, replicate = r, ngroup=ng, nrep=nrep)
        if (i > 2)
            z[i, j] = 2*z[i-1, j ] - z[i-2, j] + x
        if (i == 2)
            z[i, j] = z[i-1, j ] + x
        if (i == 1)
            z[i, j] = x
    }
}

noise.sd = 0.1
y = numeric(N)
mu = numeric(N)
group = numeric(N)
for(r in 1:nrep)
{
    for(g in 1:ng)
    {
        i = inla.idx(1:n, n=n, group=g, replicate=r, ngroup=ng, nrep = nrep)
        y[i] = z[, g] + rnorm(n, sd=noise.sd)
        mu[i] = inla.idx(1, n=1, group=g, replicate=r, ngroup=ng, nrep=nrep)
        group[i] = g
        if (g == ng && r== nrep)
        {
            i = inla.idx((n%/%2):n, n=n, group=g, replicate=r, ngroup=ng, nrep = nrep)
            y[i] = NA
        }
    }
}
mu = as.factor(mu)
idx = rep( rep(1:n, ng), nrep)
rr = rep(1:nrep, each=ng*n)
iidx = idx
rrr=rr
ggroup =group

formula = y ~ f(idx, model="rw2", initial = 3, group = group, replicate=rr, constr=FALSE,
        control.group=list(initial=0.5*log(rho/(1-rho)), fixed=FALSE))-1 + f(iidx, model="iid", group =ggroup, replicate = rrr)
## if this is added, then constr=TRUE is required
##+ mu - 1

r = inla(formula, data = list(y=y,idx=idx,mu=mu,group=group,rr=rr, ggroup=group,rrr=rrr,iidx=iidx), verbose=T,
        control.data=list(initial = log(1/noise.sd^2), fixed=TRUE),keep=T)
