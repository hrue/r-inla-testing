n = 100
x = 1:n
x = x-mean(x)
y = x^3/50^3*3 + rnorm(n, sd = 0.1)

idx = cumsum(1.1 + rexp(n))
len = numeric(n)
for(i in 1:n) {
    if (i == 1) {
        len[i] = (idx[2] -idx[1])/2
    } else if (i == n) {
        len[i] = (idx[n]-idx[n-1])/2
    } else {
        len[i] = (idx[i+1]-idx[i-1])/2
    }
}
A2 = list(
        A = rbind(len, cumsum(len)), 
        e = rep(0, 2))
A1 = list(
        A = rbind(len),
        e = 0)

std=FALSE
r1 = inla(y ~ -1 + f(idx, model="rw1", initial =0, fixed=TRUE, scale.model=std,
        constr = FALSE,  diagonal = sqrt(.Machine$double.eps), 
        extraconstr=A1),
        data = data.frame(y=rep(NA,n),idx=idx),
        verbose=FALSE,
        control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE))))

r2 = inla(y ~ -1 + f(idx, model="rw2", initial = 0, fixed=TRUE, scale.model=std,
        constr=FALSE, diagonal = sqrt(.Machine$double.eps), 
        extraconstr=A2),
        data = data.frame(y=rep(NA,n),idx=idx),
        verbose=FALSE,
        control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE))))

print(exp(sum(log(r1$summary.random$idx$sd^2)*len)/diff(range(idx))))
print(exp(sum(log(r2$summary.random$idx$sd^2)*len)/diff(range(idx))))
