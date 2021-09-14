inla.my.update()
inla.setOption("inla.call",  "inla.work")
inla.setOption("internal.binary.mode",  TRUE)

set.seed(123)
n = 1000

## 'n' is the dimension of the linear predictor defined in the formula
## below.
n = 800

## 'm' is the number of observations of eta~, where eta~ = A eta, and
## A is a fixed m x n matrix.
m = 100

## an offset, just for testing. OOPS: the offset are defined for
## 'eta~' and NOT 'eta'!!!
myoffset = 10+runif(m)

## the covariate
z = runif(n)

## the linear predictor eta
eta = 1 + z

## define it either as matrix, or using sparseMatrix() (in package
## Matrix)
A = matrix(1:(n*m), m, n);
A = as(A, "dgTMatrix")  ## just to test...
Eta = A %*% eta + myoffset
s = 0.0001

## The observations Y can be a sparseMatrix, but there is no need
## to. However, Y will be in this case, as it enherits its properties
## from Eta.
Y = Eta + rnorm(m, sd=s)

r = inla(Y ~ 1+z,

        ## The A-matrix defined here
        control.predictor = list(A=A, compute=TRUE, precision = 1e6),

        ## OOPS: we need to use a list() as the different lengths of Y
        ## and z
        data = list(Y=Y, z=z),

        ## offsets better defined here as in the formula gives the
        ## wrong impression: A formula like y ~ 1 + offset(off)
        ## suggests that the offset is in 'eta', but its in 'eta~'. Be
        ## aware.
        offset = myoffset,

        control.data = list(initial = log(1/s^2), fixed=TRUE))

## Check that the linear predictors are OK, this should be a straight
## line.
par(mfrow=c(2, 1))
plot(cbind(as.numeric(Eta), r$summary.linear.predictor[1:m,"mean"]))
plot(cbind(as.numeric(eta), r$summary.linear.predictor[m+1:n,"mean"]))
