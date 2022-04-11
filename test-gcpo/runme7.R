n <- 50
u <- rnorm(n, sd = 0.5)
eta <-  0 + u
y <- rpois(n, lambda = exp(eta))

inla.setOption(num.threads = "1:1")
inla.setOption(inla.call = "inla.mkl.work")

formula <- y ~ 1 + f(idx, model = "iid",
                     vb.correct = TRUE, 
                     hyper =  list(prec =  list(
                                       prior =  "pc.prec",
                                       param = c(0.5, 0.01))))

r <- inla(formula,
          data = data.frame(y, idx = 1:n),
          family = "poisson",
          inla.mode = "experimental", 
          control.inla = list(control.vb = list(enable = !TRUE)), 
          control.compute = list(control.gcpo = list(enable = TRUE, group.size = 1)))

rr <- inla(formula,
          data = data.frame(y, idx = 1:n),
          family = "poisson",
          control.inla = list(control.vb = list(enable = FALSE),  strategy = "laplace"), 
          control.compute = list(cpo = TRUE))

rrr <- inla(formula,
          data = data.frame(y, idx = 1:n),
          family = "poisson",
          control.compute = list(cpo = TRUE))

my.par.init(1)

plot(rr$cpo$cpo, r$gcpo$gcpo, ylim = c(0, 1), xlim = c(0, 1), pch = 19)
abline(a = 0, b = 1, lwd = 3)
round(dig = 3, cbind(r$cpo$cpo, rr$cpo$cpo, rrr$cpo$cpo, r$gcpo$gcpo))
