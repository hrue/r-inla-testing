library(evd)

loc = function(scale, tail) {
    return (scale/tail)
}

my.dgev = function(x, scale, shape, ...) {
    return (dgev(x + 0*loc(scale, shape),
                 loc = loc(scale, shape),
                 scale = scale,
                 shape = shape, ...))
}


xi = 0.25
scale = 1
x = seq(0, 5*scale, len=1000)
plot(x,  my.dgev(x, scale=scale, shape = xi, log=T))
