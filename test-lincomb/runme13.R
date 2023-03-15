set.seed(123)
n <- 3
y <- 0 + rnorm(n, sd = 1)
x <- scale(rnorm(n))
xx <- scale(rnorm(n))
xx <- xx - sum(x*xx) * x / sum(x^2)
formula =  y ~ -1 + x + xx
lc = inla.make.lincombs('x' = matrix(x, n, 1))
lcc = inla.make.lincombs('xx' = matrix(xx, n, 1))
names(lcc) <- paste0("lc", n + 1:n)
r <- inla(formula, data = data.frame(y, x),
          family = "normal",
          control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))), 
          lincomb = c(lc, lcc), 
          verbose = TRUE,
          control.fixed = list(prec.intercept = 0), 
          control.compute = list(smtp = "pardiso"), 
          control.inla = list(lincomb.derived.correlation.matrix = TRUE),
          safe = FALSE, 
          num.threads = "1:1")

cbind(
    c(rep(r$summary.fixed$sd[1], n), rep(r$summary.fixed$sd[2], n)), 
    sqrt(diag(r$misc$lincomb.derived.covariance.matrix)/c(x^2, xx^2)))
