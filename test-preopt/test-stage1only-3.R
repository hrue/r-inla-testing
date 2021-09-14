#set.seed(123)
n <- 10
idx <- rep(1, n)
x <- rnorm(n, sd = 0.2)
eta <- -1 + x
y <- rpois(n, exp(eta))

inla.setOption(inla.call = "inla.valgrind")
inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(num.threads = "1:1")
##inla.setOption(num.threads = "1:1")

lc <- inla.make.lincomb(idx = 1)
formula <- y ~ 1 + f(idx, x, model = "iid", values = 1:4, constr = T, hyper = list(prec = list(initial = 0, fixed = T, param = c(10^4, 10^4))))
##formula <- y ~ 1 + x
r <- inla(formula, 
          data = data.frame(y, x, idx),
          family = "nbinomial",
          control.inla = list(control.twostage = list(stage1only = TRUE),
                              control.vb = list(enable = "auto",
                                                refinement = 2, 
                                                max.correct = 1, 
                                                verbose = TRUE)), 
          ##lincomb = lc, 
          ##selection = list(idx = 1), 
          control.fixed = list(prec = 1, prec.intercept = 1), 
          control.compute = list(cpo = T, dic = T, smtp = 'pardiso', config = TRUE), 
          twostage = TRUE, 
          verbose = TRUE,
          keep = T, debug = TRUE)

stop("XX")

rr <- inla(formula, 
          data = data.frame(y, x, idx),
          family = "nbinomial",
          selection = list(idx = 1), 
          control.predictor = list(compute = TRUE), 
          control.inla = list(strategy = "laplace"), 
          control.fixed = list(prec = 1, prec.intercept = 1), 
          control.compute = list(cpo = T, dic = T, config = T))

intercept <- r$summary.fixed[1,"mean"]
beta <- r$summary.fixed[2, "mean"]
lp <- intercept + beta * x
print(head(round(dig = 4, cbind(true = eta,
                           r = r$summary.linear.predictor[, "mean"],
                           rr = rr$summary.linear.predictor[, "mean"],
                           r.mode = r$mode$x[1:length(lp)],
                           rr.mode = rr$mode$x[1:length(lp)]))))

print(round(dig = 4, rbind(vb = r$summary.fixed[, "mean"], 
                           laplace = rr$summary.fixed[, "mean"])))

