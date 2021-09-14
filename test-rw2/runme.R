xx = c(1, 2, 5, 6)
yy = c(1, 4, 2, 3)

m = get("inla.models", INLA:::inla.get.inlaEnv())
m$latent$rw2$min.diff = NULL
assign("inla.models", m, INLA:::inla.get.inlaEnv())

n = 50
xx.values = unique(sort(c(0, 10, xx, sample(seq(0, 10, by=0.01), n,  replace=FALSE))))
r = inla(yy ~ -1 + f(xx, model="rw2", scale.model=TRUE, values = xx.values),
         family = "gaussian",
         control.family = list(hyper = list(prec = list(initial=10, fixed=TRUE))),
         data = list(xx=xx, yy=yy, xx.values=xx.values))

plot(r$summary.random$xx$ID, r$summary.random$xx$mean, type="l",
     xlim = range(xx), ylim = c(min(yy)-1, max(yy)+1))
points(xx, yy)
points(xx.values,rep(0,length(xx.values)), pch=19)

