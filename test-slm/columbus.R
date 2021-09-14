library(INLA)
library(spdep)

data(columbus)
n<-nrow(columbus)
columbus$idx<-1:nrow(columbus)
columbus$idx2<-1:nrow(columbus)#IDX to allow for a 2nd spatial effect
W<-nb2mat(col.gal.nb)
mmatrix <- model.matrix(CRIME~1+HOVAL+INC, columbus)
mmatrix2 <-  cbind(mmatrix,W %*% mmatrix[,-1])
 
e = eigen(W)$values
re.idx = which(abs(Im(e)) < 1e-6)
rho.max = 1/max(Re(e[re.idx]))
rho.min = 1/min(Re(e[re.idx]))
rho = mean(c(rho.min, rho.max))

high.log.prec = 10
zero.variance = list(prec=list(initial = high.log.prec, fixed=TRUE))
betaprec<-.01
Q.beta = diag(1,ncol(mmatrix))
Q.beta = betaprec*Q.beta %*% t(Q.beta)
Q.beta2 = diag(1,ncol(mmatrix2))
Q.beta2 = betaprec*Q.beta2 %*% t(Q.beta2)

args.slm = list(
    rho.min = -1,#rho.min 
    rho.max = rho.max,
    W = W,
    X = matrix(0, nrow(mmatrix),1),
    Q.beta = matrix(1,1,1))

args.slm2 = list(
        rho.min = rho.min,
        rho.max = rho.max,
        W = W,
        X = mmatrix,
        Q.beta = Q.beta)
 
hyper.slm = list(
        prec = list(
                prior = "loggamma",
                param = c(1, 0.01),
                initial = -4,
                fixed = FALSE), 
        rho = list(
                initial=0,
                fixed=FALSE,
                prior = "logitbeta",
                param = c(1,1)))
rhos = c()
counter = 1
for (rho.min in seq(-2, 0, len=20))
{
    args.slm2 = list(
            rho.min = rho.min,
            rho.max = rho.max,
            W = W,
            X = mmatrix,
            Q.beta = Q.beta)

    r = inla(CRIME ~ -1 + f(idx, model="slm", args.slm = args.slm2,
            hyper = hyper.slm),
            data = columbus,
            family = "gaussian",
            control.compute = list(config=TRUE),
            control.predictor = list(initial = high.log.prec, fixed=TRUE), 
            control.inla = list(
                    ##h=0.01
                    ##tolerance=1e-8
                    ##dz=0.1, int.strategy="grid", diff.logdens=1
                    ), 
            control.family = list(hyper=zero.variance),
            verbose=FALSE)

    if (TRUE) {
        r = inla.rerun(r)
    }

    if (counter == 1) {
        dev.new()
        p1 = dev.cur()
        plot(r$marginals.hyperpar[[1]], type="l", lwd=2)
        dev.new()
        p2 = dev.cur()
        plot(inla.tmarginal(function(x, low, high) x*(high-low)+low,
                            r$marginals.hyperpar[[2]],
                            low = rho.min, high=rho.max), type="l", lwd=2)
        dev.set(dev.prev())
    } else {
        dev.set(p1)
        lines(r$marginals.hyperpar[[1]], type="l", lwd=2)
        dev.set(p2)
        lines(inla.tmarginal(function(x, low, high) x*(high-low)+low, 
                             r$marginals.hyperpar[[2]],
                             low = rho.min,  high = rho.max))
    }
    counter = counter + 1

    theta = r$mode$theta[2]
    rho = 1/(1+exp(-theta))
    rho.true = rho.min + (rho.max - rho.min)*rho
    print(rho.true)
    rhos = c(rhos,  rho.true)
}
