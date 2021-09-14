## Example of shared precision parameter and unequal locations

## This is just to avoid a check to locations to close. We can fix this in the example, but its
## easier to just to that here. Remove this and then this error will appear with some
## probability.
m = get("inla.models", INLA:::inla.get.inlaEnv())
m$latent$rw2$min.diff = NULL
assign("inla.models", m, INLA:::inla.get.inlaEnv())

## Simulate two curves at different locations
n = 20
n2 = 2*n
x1 = runif(n)
x2 = runif(n)
fun = function(x) sin(x*2*pi)
y1 = fun(x1) + rnorm(n, sd=0.1)
y2 = fun(x2) + rnorm(n, sd=0.1)

y = c(y1, y2)
x12 = c(x1, x2)
## these is the union of the locations,  and we use just 'n' of the 2n for each curve
xv = sort(c(x1, x2))
## we have two curves, ie two replications,  which will share the same precision
re = rep(1:2, each = n)
## and two intercepts
intercept = as.factor(rep(1:2, each = n))

r = inla(y ~ -1 +
             f(x12, model="rw2", scale.model=TRUE, replicate = re, values = xv), 
         data = data.frame(y, x12, xv))

par(mfrow=c(2, 1))
plot(r$summary.random$x12$ID[1:n2], r$summary.random$x12$mean[1:n2], type="l")
points(x1, y1)
plot(r$summary.random$x12$ID[n2 + 1:n2], r$summary.random$x12$mean[n2 + 1:n2], type="l")
points(x2, y2)

