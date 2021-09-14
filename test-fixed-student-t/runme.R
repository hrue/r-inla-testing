## example with a student-t_dof prior for a fixed effect.

## we use that the scale mixture of a normal where the precision is
## Gamma(shape=dof/2, rate=dof/2) (see ?dgamma) is a
## student-t_dof. the trick is to use the iid-model of dimension 1,
## which is the effect of 'x', and then set the precision so its
## marginal is student-t_dof.

n = 100
x = rnorm(n)
y = x + rnorm(n, sd = 0.1)
one = rep(1, n)
dof = 1
r = inla(y ~ 1 +
        f(one, x, model="iid",
          hyper = list(prec = list(
                               prior = "loggamma",
                               param = c(dof/2, dof/2)))),
        data = data.frame(y, x, one),
        ## you may (or may not) want to improve the integration
        ## strategy to make sure the student-t_dof prior is used
        ## properly
        control.inla = list(int.strategy = "grid", diff.logdens = 10))

plot(r$marginals.random$one[[1]])
