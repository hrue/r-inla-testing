nrow = 10
ncol = 11
ntime = 12
rho = 0.9

x = array(NA,  dim = c(nrow, ncol, ntime))

for(time in 1:ntime) {
    for(j in 1:ncol) {
        for(i in 1:nrow) {
            if (time == 1) {
                x[i, j, time ] = exp(-((i-nrow/2)^2 + (j-ncol/2)^2)/2/(min(nrow, ncol)/6))
            } else {
                x[i, j, time ] = x[i, j, 1] * cos(time * pi/ntime)
            }
        }
    }
}

xloc = rep(1:nrow, ncol)
yloc = rep(1:ncol, each=nrow)
ss = cbind(xloc, yloc)
ss = ss*1.0 ## workaround for the moment

mesh = inla.mesh(ss)
spde = inla.spde(mesh)
spde.idx <- mesh$idx$loc

i = rep(1:(nrow*ncol), ntime)
time = rep(1:ntime,  each = nrow*ncol)
s = .001
y = as.vector(x) + rnorm(nrow*ncol*ntime, sd = s)

formula = y ~ f(spde.idx, model=spde, 
        group = time,  control.group = list(model="ar1"),
        hyper = list(KT=list(fixed=TRUE)))
r = inla(formula, data = data.frame(x, i, spde.idx), verbose=T,
        control.data = list(initial = log(1/s^2),  fixed=TRUE), keep=T)

require(rgl)
m = r$summary.random$spde.idx[, "mean"]
spde.n = length(m) %/% ntime
off = 0L
for(time in 1:ntime) {
    mm = m[1:spde.n + off]
    color.p = colorRampPalette(c("black", "white"),space="Lab")
    plot(old.mesh.class(mesh), mm,  color.palette = color.p,  draw.edges = FALSE)
    off = off + spde.n
}
