Qfunc = function(node, nnode, def)
{
    if (def$n < def$s) {
        return (0)
    }
    imin = min(node, nnode) -1
    imax = max(node, nnode) -1
    idiff = imax - imin

    if (idiff >= def$s) {
        return (0)
    } else if (imin <= (def$s - idiff - 1)) {
        val = (imin + 1.0)
    } else if ((imin > (def$s - idiff - 1)) && (imin < (def$n - def$s))) {
        val = (def$s - idiff)
    } else if (imin >= (def$n - def$s)) {
        val = (def$n - idiff - imin)
    } else {
        return (0)
    }
    return (val * def$prec)
}

make.Q = function(def) 
{
    Q = matrix(0, def$n, def$n)
    for(j in 1:def$n) {
        for(i in unique(pmin(def$n, j:(j+def$s)))) {
            Q[i, j] = Q[j, i] = Qfunc(i, j, def)
        }
    }
    return (inla.as.sparse(Q))
}

make.S = function(def) 
{
    m = def$s-1
    A = rbind(diag(m), matrix(-1, 1, m))
    k = floor(def$n / def$s)
    AA = A
    for(i in 1:k) {
        AA = rbind(AA, A)
    }    
    return (AA[1:def$n, ])
}

compute.scale = function(def) 
{
    Q = make.Q(def)
    S = make.S(def)
    D = inla.as.sparse(Diagonal(n = def$n, x = rep(1E-8, def$n)))
    v = diag(inla.qinv(Q + D, constr = list(A = t(S), e = rep(0, def$s-1))))
    return (exp(mean(log(v))))
}

def = list(n = 1000,  s = 10,  prec = 1)
def$prec = def$prec*compute.scale(def)
compute.scale(def)


y = rnorm(def$n)
r = inla(y ~ -1 + f(idx, model = "seasonal",  scale.model=TRUE, season.length = def$s),
         data = data.frame(y, idx = 1:def$n),
         verbose=TRUE)
