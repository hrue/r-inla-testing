library(lme4)
library(mvtnorm)
dim = 2
grid.order = 4 ## see ?GQdk

M = GQdk(dim, grid.order)
points = M[,-1]
weights = M[, 1] / dmvnorm(points)
plot(points[,1], points[,2])
idx = order(points[,1])
Design = cbind(points, weights)[idx,]

fun = function(x) (x[1]^4)*x[3] * dmvnorm(x[1:2])

sum(unlist(apply(Design, 1,  fun)))

