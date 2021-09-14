set.seed(123)
n = 100
prec.y = 100
prec.obs = 100
prec.x = 100
scale = rep(1, n)
scale = (1:n)^2
w = rep(1, n)
w = rnorm(n)
## true unobserved covariate
x = rnorm(n, sd = 1/sqrt(prec.x)) 
## the observed covariate
xobs = x + rnorm(n, sd = 1/sqrt(prec.obs*scale))
## regression model using the unobserved 'x'
intercept = 0
beta = 4
range.beta = c(3, 5)
y = intercept + beta*w*x + rnorm(n, sd = 1/sqrt(prec.y))

print(cbind(x, xobs, y))

## prior parameters
prior.prec = c(1, .01)
prior.beta = c(0, .001)

beta.fixed = FALSE
prec.obs.fixed = TRUE
prec.x.fixed = TRUE
prec.y.fixed = TRUE
if (FALSE) {
    ## NOT NEEDED ANYMORE,  from 22/2/2013

    ## since values must be increasing (if numerical)
    ord = order(xobs)
    values = xobs[ord]
    sc = scale[ord]
} else {
    values = xobs
    sc = scale
}

formula = y ~ 1 + 
    f(xobs, w, model="mec", scale = sc, values = values, 
      range = range.beta, 
      hyper = list(
              beta = list(
                      param = prior.beta,
                      initial = 0, 
                      fixed = beta.fixed
                      ),
              prec.u = list(
                      param = prior.prec,
                      initial = log(prec.obs),
                      fixed = prec.obs.fixed
                      ),
              prec.x = list(
                      param = prior.prec,
                      initial = log(prec.x),
                      fixed = prec.x.fixed
                      ),
              mean.x = list(
                      initial = 0,
                      fixed=TRUE
                      )
              )
      )

r = inla(formula, 
        data = list(y=y, values=values, sc=sc, w=w), 
        family = "gaussian",
        keep = TRUE,
        control.family = list(
                hyper = list(
                        prec = list(
                                param = prior.prec, 
                                initial = log(prec.y),
                                fixed = prec.y.fixed
                                )
                        )
                ),
        control.fixed = list(
                mean.intercept = prior.beta[1], 
                prec.intercept = prior.beta[2],
                mean = prior.beta[1],
                prec = prior.beta[2]),
        control.inla = list(
                int.strategy = "grid",
                dz = 0.5,
                diff.logdens = 10),
        verbose = TRUE
        )

r = inla.hyperpar(r, dz = 0.5, diff.logdens = 50)
summary(r)
niter = 1000

trace.prec.x = numeric(niter)
trace.prec.y = numeric(niter)
trace.prec.obs = numeric(niter)
trace.beta = numeric(niter)
trace.intercept = numeric(niter)

## code assume this
stopifnot(prior.beta[1] == 0)

for(idx in 2:niter) {

    if (TRUE) {
        ## intercept
        a = y - beta*w*x
        Q = n*prec.y + prior.beta[2]
        b = prec.y * sum(a)
        intercept = rnorm(1, mean = b/Q, sd = 1/sqrt(Q))
    }

    if (!beta.fixed) {
        ## beta
        a = y - intercept
        Q = prec.y * sum((w*x)^2) + prior.beta[2]
        b = prec.y * sum(w*x*a)
        beta = rnorm(1, mean = b/Q, sd = 1/sqrt(Q))
    }

    if (TRUE) {
        ## x
        a = y - intercept
        Q = prec.y * w^2 * beta^2 + prec.obs * scale + prec.x ## vector
        b = prec.y * a * w * beta + prec.obs * scale * xobs ## vector
        x = rnorm(n, mean = b/Q, sd = 1/sqrt(Q))
    }

    if (!prec.y.fixed) {
        ## prec.y
        A = prior.prec[1] + n/2
        B = prior.prec[2] + sum((y-(intercept + beta*w*x))^2)/2
        prec.y = rgamma(1, shape = A,  rate = B)
    }
    
    if (!prec.x.fixed) {
        ## prec.x
        A = prior.prec[1] + n/2
        B = prior.prec[2] + sum(x^2)/2
        prec.x = rgamma(1, shape = A,  rate = B)
    }
    
    if (!prec.obs.fixed) {
        ## prec.xobs
        A = prior.prec[1] + n/2
        B = prior.prec[2] + sum(scale * (xobs -x)^2)/2
        prec.obs = rgamma(1, shape = A,  rate = B)
    }
    
    trace.prec.x[idx] = prec.x
    trace.prec.y[idx] = prec.y
    trace.prec.obs[idx] = prec.obs
    trace.beta[idx] = beta
    trace.intercept[idx] = intercept
}

par(mfrow = c(2, 2))
plot(r$marginals.fixed$'(Intercept)', type="l", lwd=2)
lines(density(trace.intercept))
title("intercept")

if (!beta.fixed) {
    plot(r$marginals.hyperpar$'MEC beta for xobs', type="l", lwd=2)
    lines(density(trace.beta))
    title("beta")
}
    
if (!prec.y.fixed) {
    plot(r$internal.marginals.hyperpar$'Log precision for the Gaussian observations',
         type="l", lwd=2)
    lines(density(log(trace.prec.y)))
    title("log(prec.y)")
}

if (!prec.x.fixed) {
    plot(r$internal.marginals.hyperpar$'MEC prec_x_intern for xobs', 
         type="l", lwd=2)
    lines(density(log(trace.prec.x)))
    title("log(prec.x)")
}

if (!prec.obs.fixed) {
    plot(r$internal.marginals.hyperpar$'MEC prec_obs_intern for xobs', 
         type="l", lwd=2)
    lines(density(log(trace.prec.obs)))
    title("log(prec.obs)")
}
