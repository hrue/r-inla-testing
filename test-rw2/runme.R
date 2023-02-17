xx = c(1, 2, 5, 6, 10, 15, 20)
yy = c(1, 4, 3, 3, 4, 2, 0)

n = 100
xx.values = unique(sort(c(0, 20, xx, sample(seq(0, 20, by=0.05), n,  replace=FALSE))))
r = inla(yy ~ -1 + f(xx, model="rw2", scale.model=TRUE, values = xx.values, constr = FALSE),
         family = "gaussian",
         control.family = list(hyper = list(prec = list(initial=10, fixed=TRUE))),
         data = list(xx=xx, yy=yy, xx.values=xx.values))

plot(r$summary.random$xx$ID, r$summary.random$xx$mean, type="l", lwd = 3, ylim = c(-5, 10))
lines(r$summary.random$xx$ID, r$summary.random$xx$'0.025quant', type="l", col = "blue")
lines(r$summary.random$xx$ID, r$summary.random$xx$'0.975quant', type="l", col = "blue")
points(xx, yy, col = "red", pch = 19, cex = 2)
points(xx.values,rep(0,length(xx.values)), pch=19)

