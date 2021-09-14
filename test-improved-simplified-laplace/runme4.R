inla.setOption(num.threads = "1:1")
intercept = 0
n = 10
ita = intercept + arima.sim(model = list(ar = 0.8), n = n, sd = 1/sqrt(1))
y = rpois(n,lambda = exp(ita))
formula=y~1+f(idx,model="ar1")
res1=inla(formula,data= list(idx = 1:n, y=y),
          family = "poisson",
          control.compute = list(config = TRUE),
          control.predictor = list(compute = TRUE),
          control.inla = list(int.strategy = "eb",improved.simplified.laplace	= T),
          verbose = F)
res2 = inla(formula,data= list(idx = 1:n, y=y),
            family = "poisson",
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE),
            control.inla = list(int.strategy = "eb",improved.simplified.laplace	= F),
            verbose = F)
res2$misc$configs$config[[1]]$improved.mean - res1$misc$configs$config[[1]]$improved.mean
res2$misc$configs$config[[1]]$mean - res1$misc$configs$config[[1]]$mean
res1$logfile[grep("^NEW", res1$logfile)]
res2$logfile[grep("^NEW", res2$logfile)]
