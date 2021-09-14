rgeneric.linear.regression =
    function(cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                     "log.prior", "quit"),
             theta = NULL)
{
    envir = parent.env(environment())

    ## artifical high precision to be added to the mean-model
    prec.high = exp(15)
    
    interpret.theta = function() {
        return(list(a = theta[1L], b = theta[2L]))
    }
    
    graph = function() {
        G = Diagonal(n = length(x), x=1)
        return(G)
    } 
    
    Q = function() {
        Q = prec.high * graph()
        return(Q)
    }
    
    mu = function() {
        par = interpret.theta()
        return(par$a + par$b * x)
    }

    log.norm.const = function() {
        return(numeric(0))
    }

    log.prior = function() {
        par = interpret.theta()
        val = (dnorm(par$a, mean=0, sd=1, log=TRUE) +
               dnorm(par$b, mean=0, sd=1, log=TRUE))
        return(val)
    }

    initial = function() {
        return(rep(0, 2))
    }
    
    quit = function() {
        return(invisible())
    }

    val = do.call(match.arg(cmd), args = list())
    return(val)
}

a = 1
b = 2
n = 50
x = rnorm(n)
eta = a + b*x
s = 0.25
y = eta + rnorm(n, sd=s)
zeros = rep(0, n)

rgen = inla.rgeneric.define(model = rgeneric.linear.regression, x=x, debug=FALSE)
r1 = inla(y ~ -1 + f(idx, model=rgen), 
         data = data.frame(y=c(y), idx = 1:n))
rr1 = inla(y ~ 1 + x, 
         data = data.frame(y, x))


rgen = inla.rgeneric.define(model = rgeneric.linear.regression, x=c(x, zeros), debug=FALSE)
rgen2 = inla.rgeneric.define(model = rgeneric.linear.regression, x=c(zeros, x), debug=FALSE)
r2 = inla(y ~ -1 + f(idx, model=rgen) + f(idx2, model=rgen2), 
         data = data.frame(y=c(y, y), idx = 1:2*n, idx2 = 1:2*n))
rgen = inla.rgeneric.define(model = rgeneric.linear.regression, x=c(x, zeros, zeros))
rgen2 = inla.rgeneric.define(model = rgeneric.linear.regression, x=c(zeros, x, zeros))
rgen3 = inla.rgeneric.define(model = rgeneric.linear.regression, x=c(zeros, zeros, x))
r3 = inla(y ~ -1 + f(idx, model=rgen) + f(idx2, model=rgen2) + f(idx3, model=rgen3),
          data = data.frame(y=c(y, y, y), idx = 1:3*n, idx2 = 1:3*n, idx3 = 1:3*n),
          verbose=TRUE)
rr = inla(y ~ 1 + x,
          data = data.frame(y, x),
          control.fixed = list(prec.intercept = 1, prec = 1))
