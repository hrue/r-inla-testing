library(rgl)
library(INLA)

map = read.table("Leuk.map")
data(Leuk)
Leuk$id = 1:dim(Leuk)[1]

plot.map = function(map, z, border=FALSE, ...)
{
    if (border) {
        rgl.lines(c(0,1),c(0,0),c(0,0))
        rgl.lines(c(1,1),c(0,1),c(0,0))
        rgl.lines(c(1,0),c(1,1),c(0,0))
        rgl.lines(c(0,0),c(1,0),c(0,0))
    }
    N = dim(map)[1]
    i = 1
    x = c()
    y = c()
    while(i < N)
    {
        n = map[i,2]
        x = c(x, NA, map[i + 1:n,1])
        y = c(y, NA, map[i + 1:n,2])
        i = i + n + 1
    }
    if (missing(z)) {
        z = rep(0,length(x))
        z[is.na(x)] = NA
    }
    rgl.linestrips(x,y,z, ...)
}


##inla.my.update()
inla.setOption("fmesher.arg", "")
inla.setOption("inla.call", "inla")

## 'round' the locations
##Leuk$xcoord = inla.group(Leuk$xcoord, 30)
##Leuk$ycoord = inla.group(Leuk$ycoord, 30)

ss = cbind(Leuk$xcoord, Leuk$ycoord)
fmesher = inla.mesh.create(ss)
Leuk$spde.idx = fmesher$locations.idx
spde = INLA:::inla.create.spde(prefix=fmesher$prefix, fem=2)

formula = inla.surv(Leuk$time, Leuk$cens) ~ 1 + sex + age + wbc + tpi +
    f(spde.idx, model="spde", spde.prefix = spde$prefix, n = spde$n, param = c(1,100,1,100,NA,NA,NA,NA))

r  = inla(formula, family="weibull", data = Leuk,  control.data = list(param=c(1,10)),
        verbose=TRUE, keep=TRUE)

tv = fmesher$mesh$tv
s = fmesher$mesh$s
zz = r$summary.random$spde.idx$mean
z = (zz-min(zz))/(max(zz)-min(zz))
cs = z
s[,3] = 0

ncolor = 512
ics = as.numeric(cut(cs, ncolor))
color = gray((1:ncolor -1)/(ncolor-1))[ics]
color = heat.colors(ncolor)[ics]

## then the interpolated surface with the map
rgl.open()
plot.map(map, color = "blue", lwd=3)
plot.inla.trimesh(tv, s, color=color, lwd=2)
rgl.viewpoint(0,0,fov=20)

rgl.open()
plot.map(map, color = "blue", lwd=3)
ss = s; ss[,3] = z - mean(z)  ## to get a nice effect
plot.inla.trimesh(tv, ss, color=color, lwd=2)

## plot the mesh
plot(fmesher)
