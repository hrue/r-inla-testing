n = 5
X = matrix((1:(n^2))^2, n, 3)
x1 = X[, 1]
x2 = X[, 2]
x3 = X[, 3]

cf = list(prec = list(x1 = 1, x2 = 2, x3 = 3))

r = inla(y ~ -1 + x1 + x2 + x3,
         data = data.frame(y = rep(NA, n), x1, x2, x3), 
         control.compute = list(smtp = "taucs", config = TRUE),
         control.predictor = list(initial = 0, fixed = TRUE), 
         control.fixed = cf, 
         control.family = list(hyper = list(prec = list(initial = 0,
                                                        fixed = TRUE))),
         control.inla = list(int.strategy = "eb",
                             reordering = "identity"))
Q = inla.as.sparse(r$misc$configs$config[[1]]$Q)
print(Q)

rr = inla(y ~ -1 + x3 + x2 + x1,
         data = data.frame(y = rep(NA, n), x1, x2, x3), 
         control.compute = list(smtp = "band", config = TRUE),
         control.predictor = list(initial = 0, fixed = TRUE), 
         control.fixed = cf, 
         control.family = list(hyper = list(prec = list(initial = 0,
                                                        fixed = TRUE))),
         control.inla = list(int.strategy = "eb",
                             reordering = "identity"))
QQ = inla.as.sparse(rr$misc$configs$config[[1]]$Q)
print(QQ)


II = diag(n)
D = diag(x = 1:ncol(X), ncol(X), ncol(X))
QA = cbind(rbind(II, -t(X)), rbind(-X,  t(X) %*% X + D))
QA[!upper.tri(QA, diag=TRUE)] = 0
QA = inla.as.sparse(QA)
print(QA)

II = diag(n)
XX = cbind(x3, x2, x1)
D = diag(x = rev(1:ncol(XX)), ncol(XX), ncol(XX))
QB = cbind(rbind(II, -t(XX)), rbind(-XX,  t(XX) %*% XX + D))
QB[!upper.tri(QB, diag=TRUE)] = 0
QB = inla.as.sparse(QB)
print(QB)


Q-QA
QQ-QB






