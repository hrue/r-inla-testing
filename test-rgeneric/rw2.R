rgeneric.rw2 = function(cmd = c("graph", "Q","mu", "initial", "log.norm.const", "log.prior",
                                "quit"),
                        theta = NULL)
{
    envir = parent.env(environment())

    mu = function() {
        return(numeric(0))
    }

    graph = function() {
        G = Q()
        G[G != 0] = 1
        return (inla.as.sparse(G))
    }

    Q = function(){
        return (exp(theta[1]) * INLA:::inla.rw2(n, sparse = TRUE))
    }

    log.norm.const = function(){
        return ((n-2)*(log(1/sqrt(2*pi)) + 0.5 * theta[1]))
    }

    log.prior = function(){
        lprior = dgamma(exp(theta[1]), shape=1, rate= 5e-5, log=TRUE) + theta[1]
        return (lprior)
    }

    initial = function(){
        return (4)
    }

    quit = function(){
        return ()
    }

    if (!length(theta))
        theta = initial()
    cmd = match.arg(cmd)
    val = do.call(cmd, args = list())

    return (val)
}


n = 100
z = seq(0, 6, length.out=n)
y = 10 * sin(z) + rnorm(n)
data = data.frame(y=y, z=z, idz=1:n)
formula = y~ -1 + f(idz, model="rw2",  constr = FALSE)
r = inla(formula,data=data,family="stdgaussian")

rw2model = inla.rgeneric.define(rgeneric.rw2, n=n)
formula = y ~ -1 + f(idz, model=rw2model)
rr = inla(formula, data=data, family="stdgaussian")

r$mode$theta - rr$mode$theta
r$mlik - rr$mlik
