N = 5
m = as.list(1:N)
for(i in 1:N) {
    log.prec = 0 + (i-N/2.0)*0.2

    r = inla(y ~ 1 + x, data = data.frame(y = c(0, 0, 0), x = c(-1, 0, 1)),
             control.family = list(hyper = list(prec = list(initial = log.prec,
                                                            fixed = TRUE))),
             control.fixed = list(prec.intercept = 1, prec = 1))
    m[[i]] = r
}

M = inla.merge(m,  verbose=TRUE)

par(mfrow = c(1, 2))
for(j in 1:2) {
    plot(m[[N]]$marginals.fixed[[j]], type="l")
    for(k in 1:(N-1))
        lines(m[[k]]$marginals.fixed[[j]], type="l")
    lines(M$marginals.fixed[[j]], type="l", col="blue", lwd = 3)
}

dev.new()

### *************************************

N = 5
m = as.list(1:N)
for(i in 1:N) {
    log.prec = 0 + (i-N/2.0)*0.2

    r = inla(y ~ -1 + f(x, initial = 0, fixed=T),
             data = data.frame(y = c(0, 0, 0), x = c(-1, 0, 1)),
             control.family = list(hyper = list(prec = list(initial = log.prec,
                                                            fixed = TRUE))))
    m[[i]] = r
}

M = inla.merge(m, verbose=TRUE)
par(mfrow = c(1, 3))
for(j in 1:3) {
    plot(m[[N]]$marginals.random$x[[j]], type="l")
    for(k in 1:(N-1))
        lines(m[[k]]$marginals.random$x[[j]], type="l")
    lines(M$marginals.random$x[[j]], type="l", col="blue", lwd = 3)
}


