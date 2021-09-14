n = 100
y = rexp(n)
z = rnorm(n)
strata = sample(1:3, n, replace=TRUE)

formula = inla.surv(y) ~ z
r = inla(formula, family = "piecewise.constant",
        control.hazard = list(strata.name="strata"),
        data = data.frame(y,z,strata))

