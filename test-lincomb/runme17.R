m <- 10
n = m^2
a = rnorm(n)
b = rnorm(n)
idx = sort(rep(c(1:m), m))
y = 1 + a + b + rnorm(n)
df <- data.frame(y = y, a = a, b = b, idx = idx)
formula = y ~ 1 + a + b + f(idx, model="iid")
lc1 <- inla.make.lincombs("(Intercept)" = cbind(rep(1, m)), idx = diag(m))
r = inla(formula, data = df,
    	lincomb = lc1,
    	control.family = list(initial=10, fixed=TRUE),
        control.compute = list(config = TRUE))
lc1.1 = r$summary.fixed["(Intercept)", "mean"] + r$summary.random$idx$mean  ## Point estimates
lc1.2 = unique(r$summary.lincomb.derived$mean)
cbind(lc1.1 = lc1.1, lc1.2 = lc1.2)
