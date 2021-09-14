## This is an example from Kim.v.K

## the model is
##
## log(mu1)=log(n_i)+log(1-exp(-lambda_i 2^{rho_i}))
## log(mu2)=log(n_i)+log(exp(-lambda_i 2^{rho_i})-exp(-lambda_i 10^{rho_i}))
##
## with poisson counts, where rho_i and lambda_i depends  linearly on covariates

n = 1000
X = matrix(runif(n * 4, min= -0.2, max=.2), n, 4) ## the covariates

rho = 1 + .2 * X[, 1] +.3 * X[, 2]
lambda = 1 + .22 * X[, 3] + .33 * X[, 4]

eta.1 = 5 + log(1-exp(-lambda * 2^rho))
eta.2 = 5 + log(exp(-lambda * 2^rho) - exp(-lambda * 10^rho))

y1 = rpois(n, exp(eta.1))
y2 = rpois(n, exp(eta.2))
y = c(y1, y2)
intercept = as.factor(rep(1:2, each=n))

rgeneric.kim =
    function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const",
                      "log.prior", "quit"),
              theta = NULL, args = NULL)
{
    ## artifical high precision to be added to the mean-model
    prec.high = exp(15)
    graph = function(theta, X, n) {
        G = Diagonal(n = 2*n, x=1)
        return(G)
    }
    Q = function(theta, X, n) {
        Q = prec.high * graph(theta, X, n)
        return(Q)
    }
    mu = function(theta, X, n) {
        rho = exp(theta[1]) + exp(theta[2]) * X[, 1] + exp(theta[3]) * X[, 2]
        lambda = exp(theta[4]) + exp(theta[5]) * X[, 3] + exp(theta[6]) * X[, 4]
        eta.1 = log(1-exp(-lambda * 2^rho))
        eta.2 = log(exp(-lambda * 2^rho) - exp(-lambda * 10^rho))
        return (c(eta.1, eta.2))
    }
    log.norm.const = function(theta, X, n) {
        ## let INLA compute this
        return(numeric(0))
    }
    log.prior = function(theta, X, n) {
        return (sum(dnorm(theta, mean=0, sd=1, log=TRUE)))
    }
    initial = function(theta, X, n) {
        return(rep(0, 6))
    }
    quit = function(theta, X, n) {
        return(invisible())
    }
    cmd = match.arg(cmd)
    val = do.call(cmd, args = list(theta = theta, X = args$X, n = dim(args$X)[1]))
    return(val)
}



rgen = inla.rgeneric.define(model = rgeneric.kim, X=X)
r = inla(y ~ -1 + intercept + f(idx, model = rgen),
         data = data.frame(intercept, idx = 1:(2*n)),
         family = "poisson",
         verbose=TRUE)

print(exp(r$summary.hyperpar[, "mode"]))
