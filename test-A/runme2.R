n = 100
z = rnorm(n)
zz = 1:n
lin.pred = z + zz
y = numeric(n)
lin.pred.new = numeric(n)
A=list(i=c(), j=c(), values=c())
for(i in 1:n) {
    lin.pred.new[i] = mean(lin.pred[1:i])
    y[i] = lin.pred.new[i] + rnorm(1, sd=0.1)
    A$i = c(A$i, rep(i, i))
    A$j = c(A$j, 1:i)
    A$values = c(A$values, rep(1/i, i))
}
Am = sparseMatrix(i = A$i, j = A$j, x = A$values)
formula = y ~ z + zz
r = inla(formula, data = data.frame(y,z,zz),
        control.predictor = list(compute=T, A = Am))

par(mfrow=c(2,2))


## the internal linear.predictor is a vector of 2*n, where the first n
## are lin.pred.new and the last n is lin.pred.
plot(r$summary.linear.predictor$mean[1:n], lin.pred.new)
plot(r$summary.linear.predictor$mean[1:n + n], lin.pred)
