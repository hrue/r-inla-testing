n = 50
y = yy = rnorm(1)

## do n-chunks of each m data,  sequencially
r = inla(y ~ -1, data = data.frame(y=y),
        control.family = list(param = c(1, 1)))
mlik = r$mlik
plot(r$internal.marginals.hyperpar[[1]],
     ylim = c(0,
             10*max(r$internal.marginals.hyperpar[[1]][, 2])))
for(i in 2:n) {
    print(i)
    y = rnorm(1)
    yy = c(yy, y)
    rr = inla(y ~ -1,
              data = data.frame(y=y),
              control.update = list(result = r),
              control.mode = list(theta = r$mode$theta,  restart=TRUE))
    r = rr
    mlik = mlik + r$mlik
    lines(r$internal.marginals.hyperpar[[1]])
}
lines(r$internal.marginals.hyperpar[[1]], lwd=3, col="red")

## do all data in one chunk
rr = inla(y ~ -1, data = data.frame(y=yy),
        control.family = list(param = c(1, 1)))
lines(rr$internal.marginals.hyperpar[[1]], lwd=5, col="blue")

## compare results
dev.new()
plot(r$internal.marginals.hyperpar[[1]], main="Comparing joint and sequencial")
lines(rr$internal.marginals.hyperpar[[1]])

print(cbind(sequential = rr$mlik[1], joint=mlik[1],
            difference = rr$mlik[1]-mlik[1]))
