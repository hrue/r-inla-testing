n = 100
beta = arima.sim(n, model = list(ar = 0.99))
beta = scale(beta)
x = rnorm(n)
y = 1 + beta*x + rnorm(n, sd=0.1)

r = (inla(y ~ 1 + f(idx, x, model="ar1",
                    hyper = list(
                        prec = list(
                            prior = "pc.prec",
                            param = c(3, 0.01)),
                        rho = list(
                            prior = "pc.rho1",
                            param = c(0.5,  0.51)))),
          verbose=TRUE, 
          data = data.frame(y, idx=1:n)))


