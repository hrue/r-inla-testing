library(splines2)
n <- 10
s <- 0.1
x <- 1:10
y <- sin(12 * x/n)  + rnorm(n, sd=s)
plot(x, y, pch=19)


knots <- quantile(x,seq(0.05,0.95,by=0.2))
bsMat <- bSpline(x, knots = knots,  degree = 1, intercept = FALSE)
values <- sort(c(bsMat$Boundary.knots, bsMat$knots))
class(bsMat) <- "matrix"
bsMat <- inla.as.sparse(bsMat)[,-1]
r <- inla(y ~ 1 + f(idx, model="rw2", A.local=bsMat, values=values),
	family="gaussian",
	control.family=list(hyper=list(prec=list(initial=log(1/s^2),fixed=TRUE))),
	data=list(y=y,idx=rep(NA,n),values=values,bsMat=bsMat))
