inla.my.update()
inla.setOption("fmesher.call", "fmesher")

n = 10 
locations = list(x = runif(n), y=runif(n), z=NULL)
dir = inla.create.matern(data.locations = locations)

print(dir)

