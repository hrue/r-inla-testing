nrow = 10
ncol = 11
ntime = 20
rho = 0.9

x = array(NA,  dim = c(nrow, ncol, ntime))

for(time in 1:ntime) {
    for(j in 1:ncol) {
        for(i in 1:nrow) {
            
            if (time == 1) {
                x[i, j, time ] = exp(-((i-nrow/2)^2 + (j-ncol/2)^2)/2/4)
            } else {
                x[i, j, time ] = x[i, j, 1] * cos(time * pi/ntime)
            }
            
        }
    }
}

i = rep(1:(nrow*ncol), ntime)
time = rep(1:ntime,  each = nrow*ncol)
s = .1
y = as.vector(x) + rnorm(nrow*ncol*ntime, sd = s)

formula = y ~ f(i, model="matern2d", nrow=nrow, ncol=ncol, group = time,  control.group = list(model="ar1")) 
r = inla(formula, data = data.frame(x, i), verbose=T,
        control.family = list(initial = log(1/s^2),  fixed=TRUE))

## estimate
for(time in 1:ntime) {
    dev.new()
    offset = (time -1L) * nrow * ncol
    inla.display.matrix(matrix( r$summary.random$i$mean[1:(nrow*ncol) + offset], nrow,ncol))
}

## truth
xx = as.vector(x)
for(time in 1:ntime) {
    dev.new()
    offset = (time -1L) * nrow * ncol
    inla.display.matrix(matrix( xx[1:(nrow*ncol) + offset], nrow,ncol))
}
