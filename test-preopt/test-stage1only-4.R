#set.seed(123)
n <- 30
idx <- rep(1, n)
x <- rnorm(n, sd = 0.2)
off <- (1:n-n/2)/n 
eta <- -1 + x + off
y <- rpois(n, exp(eta))
y[2] <- NA

inla.setOption(inla.call = "inla.valgrind")
inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(num.threads = "1:1")
##inla.setOption(num.threads = "1:1")

lc <- inla.make.lincomb(idx = 1)
formula <- y ~ 1 + f(idx, x, model = "iid", vb.correct = 1, values = 1:4, constr = F, hyper = list(prec = list(initial = 0, fixed = T, param = c(10^4, 10^4))))
##formula <- y ~ 1 + x
r <- inla(formula,
          offset = off, 
          data = data.frame(y, x, idx, off = off),
          family = "nbinomial",
          control.inla = list(control.twostage = list(stage1only = TRUE),
                              control.vb = list(enable = "auto",
                                                refinement = 2, 
                                                max.correct = 1, 
                                                verbose = TRUE),
                              lincomb.derived.correlation.matrix = TRUE), 
          ##lincomb = lc, 
          ##selection = list(idx = 2), 
          control.fixed = list(prec = 1, prec.intercept = 1), 
          control.compute = list(cpo = F, dic = T, smtp = 'pardiso', config = TRUE), 
          twostage = TRUE, 
          verbose = TRUE,
          keep = T)


rr <- inla(formula, 
          offset = off, 
          data = data.frame(y, x, idx, off),
          family = "nbinomial",
          verbose = F, 
          lincomb = lc, 
          selection = list(idx = 2), 
          control.predictor = list(compute = TRUE), 
          control.inla = list(strategy = "laplace"), 
          control.fixed = list(prec = 1, prec.intercept = 1), 
          control.compute = list(cpo = T, dic = T, config = T))

