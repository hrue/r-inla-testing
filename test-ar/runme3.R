inla.setOption(num.threads=8)

set.seed(123)
n = 1000
phi = 0.9
y = arima.sim(n, model = list(ar = phi))
y = scale(y)*2
idx = 1:n
param.phi = c(0, 1)
param.prec = c(1, 0.01)

ar1 = function() {
    inla(y ~ -1 + f(idx, model='ar1',
                    hyper = list(
                            prec = list(initial = 1, fixed=FALSE, param = param.prec),
                            rho = list(initial = 3, fixed=FALSE, param = param.phi))),
         family = "gaussian", 
         control.family = list(initial = 5, fixed=TRUE), 
         data = data.frame(y, idx),
         keep=TRUE, 
         verbose =TRUE)
}

ar = function() {
    inla(y ~ -1 + f(idx, model='ar',
                    order = 1, 
                    hyper = list(
                            prec = list(initial = 1, fixed=FALSE, param = param.prec),
                            theta2 = list(initial = 3, fixed=FALSE, param = param.phi))), 
         family = "gaussian", 
         control.family = list(initial = 5, fixed=TRUE), 
         data = data.frame(y, idx),
         keep=TRUE, 
         verbose=TRUE)
}

                
                
