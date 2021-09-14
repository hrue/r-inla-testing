y = 1:3
xx = factor(c(NA, "a", 1))
xa = c(NA, 1, NA)
x1 = c(NA, NA, 1)
XX = matrix(runif(9), 3, 3)
colnames(XX) = paste("col", 1:3,  sep="")
diag(XX) = NA
idx = 1:3

r = inla(y ~ -1 + xx + XX + f(idx),  data = data.frame(xx=xx, y=y, XX=XX, idx=idx))
rr = inla(y ~ 1 + xa + x1 + XX + f(idx),  data = data.frame(xa=xa, x1=x1, y=y, XX=XX, idx=idx))
