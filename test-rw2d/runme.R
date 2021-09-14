DFT2 = function(x)
{
    fft(x,inverse=FALSE)/sqrt(length(x))
}

IDFT2 = function(x)
{
    fft(x,inverse=TRUE)/sqrt(length(x))
}

make.base.1 = function(size, delta = 0)
{
    if (length(size) == 1) size = c(size, size)
    base      = matrix(data=0, nrow=size[1], ncol=size[2])
    base[1,1] = 4 + delta
    base[1,2] = base[2,1] = base[size[1],1] = base[1,size[2]] = -1
    return(base)
}

eigenvalues = function(base)
{
    return (Re(sqrt(length(base))*DFT2(base)))
}

make.base = function(size, delta = 0)
{
    base = make.base.1(size, delta)
    base2 = Re(IDFT2(DFT2(base)^2)) * sqrt(length(base))
    return(base2)
}

inverse = function(base, rankdef, factor = 1000)
{
    eig = Re(DFT2(base))
    eig.max = max(eig)
    if (missing(rankdef)) {
        pos = eig > eig.max * factor * .Machine$double.eps
    } else {
        eig.sort = sort(eig)
        if (rankdef > 0) {
            pos = eig > eig.sort[rankdef]
        } else {
            pos = eig > 0
        }
    }
    eig[pos] = 1/eig[pos]
    eig[!pos] = 0
    base.inv = Re(IDFT2(eig))/length(base)
    return (base.inv)
}

tozero = function(base, factor = 100)
{
    elm.max = max(abs(base))
    base[abs(base) < factor*.Machine$double.eps*elm.max] = 0
    return(base)
}
    
mult = function(base, bbase)
{
    ab = DFT2(base) * DFT2(bbase)
    xx = Re(IDFT2(ab)) * sqrt(length(base))
    return(xx)
}
    
do.sample = function(base, rankdef = 0)
{
    eig = pmax(0, eigenvalues(base))
    eig.max = max(eig)
    if (missing(rankdef)) {
        pos = (eig > eig.max * 1000*.Machine$double.eps)
    } else {
        eig.sort = sort(eig)
        if (rankdef > 0) {
            pos = eig > eig.sort[rankdef]
        } else {
            pos = eig > 0
        }
    }
    eig[pos] = 1/sqrt(eig[pos])
    eig[!pos] = 0

    d = dim(base)
    n = prod(d)
    tmp = eig * matrix(complex(real = rnorm(n),  imag = rnorm(n)), d[1], d[2])
    xx = Re(DFT2(tmp))
    return (xx)
}


nrow = ncol = 64
base = make.base(c(nrow, ncol))
base = base * inverse(base, rankdef=1)[1, 1]

s = 3
xx = do.sample(base, rankdef=1)
yy = xx + matrix(rnorm(nrow*ncol, sd = s), nrow, ncol)
var.tot = 1 + s^2
y = c(yy)/sqrt(var.tot)
idx = 1:(nrow*ncol)

formula = y ~ -1+f(idx, model="rw2diid", nrow=nrow, ncol=ncol, cyclic = FALSE, constr=FALSE,
    hyper = list(theta2 = list(param = c(0.5, 0.99))))
r = inla(formula, data = data.frame(y, idx),
        verbose=TRUE,
        control.family = list(hyper = list(prec = list(initial = 13, fixed=TRUE))))

if (FALSE) {
    fformula = y ~ -1+f(idx, model="rw2d", nrow=nrow, ncol=ncol, cyclic = TRUE, constr=FALSE, 
            diagonal=0, 
            initial = 0, fixed=FALSE, prior = "pc.prec", param = c(3.1, 0.01), scale.model=TRUE)
    rr = inla(fformula, data = data.frame(y, idx),
            verbose=TRUE,
            control.family = list(hyper = list(prec = list(initial = 13, fixed=TRUE))))
    plot(r$marginals.hyperpar[[1]])
    lines(rr$marginals.hyperpar[[1]])
}



