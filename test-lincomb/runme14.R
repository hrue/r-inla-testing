set.seed(123)
n <- 10
y <- rep(0, n)
idx <- 1:n
formula =  y ~ 1 + f(idx, model = "iid", hyper = list(prec = list(initial = 0, fixed = TRUE)))
M <- diag(1:n)^2
lc = inla.make.lincombs('idx' = M)
r <- inla(formula, data = data.frame(y, idx),
          family = "normal",
          control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))), 
          lincomb = lc, 
          verbose = TRUE,
          control.fixed = list(prec.intercept = 0), 
          ##control.compute = list(smtp = "pardiso"), 
          control.inla = list(lincomb.derived.correlation.matrix = TRUE),
          safe = FALSE, 
          num.threads = "1:1")
cbind(sqrt(diag(r$misc$lincomb.derived.covariance.matrix)), 
      sqrt(r$summary.random$idx$sd^2 * diag(M)^2))
