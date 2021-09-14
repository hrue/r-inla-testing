##set.seed(123)
n <- 133
m <- 121
x <- rnorm(n)
xx <- rnorm(n)
xxx <- rnorm(n)
s <- 0.01
x[c(1, 2, 3)] <- 0
xx[c(3, 5)] <- 0
A <- matrix(runif(n*m), m, n)
for(i in 1:nrow(A)) {
    while(sum(mask <- sample(0:1, ncol(A), prob = c(1,2/n), replace = TRUE)) == 0) {
        ## do nothing
    }
    A[i, ] <- mask * A[i, ]
    A[i, ] <- A[i, ]/sum(A[i, ])
}
stopifnot(all(rowSums(A != 0) >0))
A <- inla.as.sparse(A)

y <- A %*% cbind(1+x+xx+xxx) + rnorm(m, sd = 0.1)
##y <- A %*% cbind(1+x+xx) + rnorm(m, sd = 0.1)

pAA <- rbind(A %*% cbind(1, x, xx, xxx), cbind(1, x, xx, xxx))
##pAA <- rbind(A %*% cbind(1, x, xx), cbind(1, x, xx))

inla.setOption(num.threads = "1:1")

inla.setOption(inla.call = "inla.mkl.work")
##inla.setOption(inla.call = "inla.valgrind")
big <- 20
i.xx <- c(1, NA, 1, NA, 1)
j.xx <- c(1, 1, NA, NA, 1)
i.xx <- rep(1, n)
j.xx <- rep(1, n)

rr <- inla(y ~ 1 + x+ xx + xxx, 
               ##f(i.xx, xx, model = "iid",
               ##   hyper = list(prec = list(initial = 0, fixed = TRUE))), 
               ##+f(j.xx, xxx, copy = "i.xx"), 
           data = list(y = y, x = x, xx = xx, xxx = xxx, i.xx = i.xx, j.xx = j.xx),
           family = "normal", 
           verbose = TRUE, 
           control.compute = list(smtp = 'pardiso', dic = T, cpo = T, po = T, config = TRUE), 
           control.inla = list(cmin = 0, int.strategy = "eb", control.twostage = list(stage1only = TRUE), verbose = T), 
           control.predictor = list(A = A, hyper = list(prec = list(initial = big))), 
           control.fixed = list(prec.intercept = 1, prec = 1), 
           control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))), 
           control.expert = list(disable.gaussian.check = TRUE), 
           twostage = TRUE)
rr <- inla.rerun(rr)
AA <- A
r <- inla(y ~ 1 + x+xx+xxx, 
          ##f(i.xx, xx, model = "iid",
          ##      hyper = list(prec = list(initial = 0, fixed = TRUE))), 
          ##+f(j.xx, xxx, copy = "i.xx"), 
          data = list(y = y, x = x, xx = xx, xxx = xxx, i.xx = i.xx, j.xx = j.xx),
          family = "normal",
          verbose = T, 
          ##control.mode = list(result = r, restart = TRUE), 
          control.compute = list(smtp = 'pardiso', dic = T, cpo = T, po = T, config = T), 
          control.inla = list(cmin = 0, int.strategy = "eb", strategy = "gaussian", verbose = T), 
          control.predictor = list(A = AA, precision = exp(big),
                                   compute = T, hyper = list(prec = list(initial = big))), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))),
          control.expert = list(disable.gaussian.check = TRUE))
r <- inla.rerun(r)
r$mlik - rr$mlik
r$summary.fixed - rr$summary.fixed

par(mfrow = c(1, 2))
plot(r$summary.linear.predictor[, "mean"],
     rr$summary.linear.predictor[, "mean"], pch = 19, col = "blue")
abline(a = 0, b = 1, lwd = 2)
plot(r$summary.linear.predictor[, "sd"], 
     rr$summary.linear.predictor[, "sd"], pch = 19, col = "blue")
abline(a = 0, b = 1, lwd = 3)

print(cor(rr$summary.linear.predictor[, "mean"],r$summary.linear.predictor[, "mean"]))
print(cor(rr$summary.linear.predictor[, "sd"],r$summary.linear.predictor[, "sd"]))
print(round(dig = 4, cbind(rr$summary.linear.predictor[, "sd"],r$summary.linear.predictor[, "sd"])))

print(cor(r$dic$local.dic, rr$dic$local.dic))
print(cor(r$cpo$cpo, rr$cpo$cpo))
print(cor(r$po$po, rr$po$po))

r.mode <- r$mode$x[-(1:(n+m))]
rr.mode <- rr$mode$x[-(1:(n+m))]

r$mode$x - c(as.numeric(pAA %*% r.mode), r.mode)
rr$mode$x - c(as.numeric(pAA %*% rr.mode), rr.mode)

pAA <- rbind(A %*% cbind(1, x, xx))
##print(round(dig = 4, pAA))


Qinv <- r$misc$configs$config[[1]]$Qinv
Qinv <- Qinv + t(Qinv)
diag(Qinv) <- diag(Qinv)/2


r.sim <- inla.posterior.sample(5000, r)
rr.sim <- inla.posterior.sample(5000, rr)

idx <- c(1, 2, 3, 4)

par(mfrow = c(2, 2))

p.sim <- inla.posterior.sample.eval(function() Predictor[idx], r.sim)
pp.sim <- inla.posterior.sample.eval(function() Predictor[idx], rr.sim)

for(i in seq_along(idx)) {
    hist(p.sim[i, ], n = 300, prob = TRUE)
    lines(density(pp.sim[i, ]), lwd = 3, col = "blue")
}
