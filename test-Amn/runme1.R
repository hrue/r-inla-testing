set.seed(123)
n = 30
m = 10
off = 10+runif(m)
z = runif(n)
eta = 1 + z
A = matrix(1:(n*m),m,n)
s = 0.0001
Eta = A %*% eta + off
Y = Eta +  rnorm(m, sd=s)

inla.setOption("inla.call", "/home/hrue/p/inla/work/local/bin/inla")
inla.my.update("~/p/inla/google-code/inla/rinla/R")

r = inla(Y ~ 1+z + offset(off), control.predictor = list(A=A,compute=TRUE,
                        precision = 1e6),
        data = list(Y=Y, z=z),
        control.compute=list(cpo=T),
        control.family = list(initial = log(1/s^2), fixed=TRUE),
        keep=T,verbose=T, debug=T)

print(cbind(Eta, r$summary.linear.predictor[1:m,"mean"]))
print(cbind(eta,r$summary.linear.predictor[m+1:n,"mean"]))
