g = 3
n = 30
size <- g
i = seq(1,n)
x = matrix(NA, g, n)
for (j in 1:g) {
    x[j,] = sin(i/100 + j-1) 
}

m = apply(x, 2, mean)
for(j in 1:n)
    x[,j] = x[,j] - m[j]
for(i in 1:g)
    x[i,] = x[i,] - mean(x[i, ])

y = matrix(NA, g, n)
p = matrix(NA, g, n)
for(j in 1:n) {
    p[,j] = exp(x[,j])/sum(exp(x[,j]))
    y[,j] = rmultinom(1, size=size, prob = p[,j])
}

experiment = matrix(NA, g, n)
for(j in 1:n)
    experiment[,j] = j

bin = matrix(NA, g, n)
for(j in 1:n)
    bin[,j] = 1:g

N = n*g
data = data.frame(
    y = as.vector(y),
    experiment = as.vector(experiment),
    bin = as.vector(bin), 
    intercept = as.vector(experiment))

data2 = data.frame(
    y = c(data$y, rep(NA, N)),
    experiment = c(data$experiment, data$experiment),
    bin = c(data$bin, data$bin),
    cross=c(rep(NA,N), data$experiment),
    intercept = c(data$experiment, rep(NA,N)))

rr <- list()
for (constr in c(FALSE, TRUE)) {
    formula = y ~ f(experiment,
                    model = "rw2",
                    scale.model = TRUE, replicate = bin, constr=constr) -1 + 
                  f(intercept, model="iid",
                    hyper =  list(prec = list(initial = -6, fixed=TRUE)))
    r = inla(formula,
             data = data2,
             family = "poisson",
             verbose=TRUE, 
             control.predictor = list(cross = data2$cross))

    rr <- c(rr, list(r))

    ## quick estimate
    pp = matrix(r$summary.random$experiment$mean, byrow=TRUE, g, n)
    p.est = matrix(NA,g,n)
    for(j in 1:n) 
        p.est[,j] = exp(pp[,j])/sum(exp(pp[,j]))

    inla.dev.new()
    par(mfrow = rep(ceiling(sqrt(g)), 2))
    for(i in 1:g) {
        plot(p[i, ], lwd = 3, type = "l", col = "blue", 
             ylim = c(0, 1),
             main = paste("constr=", constr))
        points(p.est[i, ], pch = 19)
    }
}

formula = y ~ -1 +
    f(experiment, model = "rw2", diagonal = 1e-5,
      scale.model = TRUE, replicate = bin, constr=FALSE) + 
    f(intercept, model="iid",
      hyper =  list(prec = list(initial = -6, fixed=TRUE)))
r = inla(formula, data = data, family = "poisson", verbose=TRUE)
rr <- c(rr, list(r))
pp = matrix(r$summary.random$experiment$mean, byrow=TRUE, g, n)
rowm <- rbind(colMeans(pp))
for(i in 1:g) {
    pp[i, ] <- pp[i, ] - rowm
}
colm <- cbind(rowMeans(pp))
for(i in 1:n) {
    pp[, i] <- pp[, i] - colm
}
p.est = matrix(NA,g,n)
for(j in 1:n) {
    p.est[,j] = exp(pp[,j])/sum(exp(pp[,j]))
}
inla.dev.new()
par(mfrow = rep(ceiling(sqrt(g)), 2))
for(i in 1:g) {
    plot(p[i, ], lwd = 3, type = "l", col = "blue", 
         ylim = c(0, 1),
         main = paste("no constr"))
    points(p.est[i, ], pch = 19)
}
