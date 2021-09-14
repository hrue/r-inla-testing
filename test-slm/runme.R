##INLA:::inla.my.update(b=T)

n = 100
m = 10 ## could also be zero!
X = matrix(rnorm(n*m), n, m)
W = matrix(rnorm(n^2), n, n)
W = W %*% t(W)
diag(W) = diag(W) * 1.1
Q.beta = matrix(rnorm(m^2), m, m)
Q.beta = 10*Q.beta %*% t(Q.beta)


e = eigen(W)$values
re.idx = which(abs(Im(e)) < 1e-6)
rho.min = 1/max(Re(e[re.idx]))
rho.max = 1/min(Re(e[re.idx]))
rho = mean(c(rho.min, rho.max))

if (m > 1) {
    beta = inla.qsample(1, Q=Q.beta)
} else {
    beta = numeric(0)
}
kappa = 10

mu = solve(diag(n) - rho*W) %*% X %*% beta
Q = kappa * (diag(n)-rho*t(W)) %*% (diag(n) - rho*W)
y = inla.qsample(1, Q=Q, mu=mu)

## predict these
pred.idx = 1:10
y.save = y[pred.idx]
y[pred.idx] = NA
r = inla(y ~ -1 + f(idx, model="slm",
        args.slm = list(
                rho.min = rho.min,
                rho.max = rho.max,
                W = W,
                X = X,
                Q.beta = Q.beta),
        hyper = list(
                prec = list(
                        prior = "loggamma", 
                        param = c(1, 0.1)), 
                rho = list(
                        prior = "logitbeta",
                        param = c(1, 1)))), 
        data = data.frame(idx = 1:n, y=y), 
        family = "gaussian",
        control.family = list(
                hyper = list(
                        prec = list(
                                initial = 10,
                                fixed = TRUE))),
        verbose=TRUE,
        keep=TRUE)

cbind(r$summary.random$idx$mean[-(1:n)],  beta)

cbind(y.save = y.save,
      y.pred.mean = r$summary.random$idx$mean[pred.idx], 
      y.pred.sd = r$summary.random$idx$sd[pred.idx],
      z = ((y.save - r$summary.random$idx$mean[pred.idx]) /
           r$summary.random$idx$sd[pred.idx]), 
      p = pnorm((y.save - r$summary.random$idx$mean[pred.idx]) /
          r$summary.random$idx$sd[pred.idx]))

