n = 50
Q = INLA:::inla.rw1(n, scale.model=TRUE)
diag(Q) = diag(Q) + 1e-6
nrep = 100
s = 0.01
y = c()
for(i in 1:nrep) {
    u = c(inla.qsample(n=1, Q=Q, constr = list(A=matrix(1, 1, n), e=0)))
    x = 1/sqrt(tau) * (sqrt(0.5) * u + sqrt(0.5) * rnorm(n))
    intercept = 3
    y = c(y, intercept + x + rnorm(n, sd = s))
}

formula = (y ~ 1 + f(region,
                     model = "bym2",
                     graph = inla.read.graph(Q),
                     constr=TRUE,
                     scale.model=TRUE,
                     compute=FALSE, 
                     replicate = r,
                     hyper=list(
                         phi=list(initial=0.1),
                         prec = list(initial=0.1))))

r = (inla(formula,
          
          family="gaussian",
          control.family = list(
              hyper=list(prec=list(initial=log(1/s^2), fixed=TRUE))), 
          verbose=TRUE,
          inla.call="remote", 
          data=data.frame(
              y,
              region = rep(1:n, nrep),
              r = rep(1:nrep, each = n))))


