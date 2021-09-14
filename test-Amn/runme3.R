library(rgl)

npoints = 50
points = matrix(rnorm(2*npoints),  npoints,  2)
fmesher = inla.fmesher.mesh(points, dir="./data", x11=0)
spde = inla.create.spde(prefix=fmesher$prefix, fem=2)

y = apply(points, 1, function(x) return (exp(-sum(x^2))))
m = npoints %/% 2L
B =  matrix(runif(npoints * m),  m,  npoints)
Y = B %*% y

B = as(B, "dgTMatrix")

formula = Y ~ f(idx,  model="spde",  spde.prefix = spde$prefix,  n = spde$n)

r = inla(formula,  data = list(Y=Y, idx = fmesher$locations.idx),
        control.predictor = list(A=B), 
        family = "gaussian",
        control.family = list(initial = 8,  fixed=TRUE),
        control.inla = list(int.strategy = "eb"))

tv = fmesher$mesh$tv
s = fmesher$mesh$s
zz = r$summary.random$idx$mean
z = (zz-min(zz))/(max(zz)-min(zz))
cs = z
s[,3] = zz

ncolor = 512
ics = as.numeric(cut(cs, ncolor))
color = gray((1:ncolor -1)/(ncolor-1))[ics]
color = heat.colors(ncolor)[ics]

## then the interpolated surface with the map
rgl.open()
plot.inla.trimesh(tv, s, color=color, lwd=2)
rgl.viewpoint(0,0,fov=20)



