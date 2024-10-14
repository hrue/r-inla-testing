##INLA:::inla.my.update(b = T)
##inla.setOption(inla.call = "inla.mkl.work")

set.seed(123)
inla.setOption(num.threads = "1:4")
n = 500000
z = rnorm(n, sd = 0.3)
eta = 1 + 1*z
N = 1


## logit
set.seed(123)
p = inla.link.invlogit(eta)
y = rbinom(n,  size = N, prob = p)
r1 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
        control.family = list(control.link = list(model = "logit")), control.predictor = list(compute=TRUE))
cat("\nlogit\n")
r1$cpu.used
r1$summary.fixed[, "mean"]

## probit
set.seed(123)
p = inla.link.invprobit(eta)
y = rbinom(n,  size = N, prob = p)
r2 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
          control.family = list(control.link = list(model = "probit")), control.predictor = list(compute=TRUE))
cat("\nprobit\n")
r2$cpu.used
r2$summary.fixed[, "mean"]

## cloglog
set.seed(123)
p = inla.link.invcloglog(eta)
y = rbinom(n,  size = N, prob = p)
r3 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
        control.family = list(control.link = list(model = "cloglog")), control.predictor = list(compute=TRUE))
cat("\ncloglog\n")
r3$cpu.used
r3$summary.fixed[, "mean"]

## loglog
set.seed(123)
p = inla.link.invloglog(eta)
y = rbinom(n,  size = N, prob = p)
r4 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
        control.family = list(control.link = list(model = "loglog")), control.predictor = list(compute=TRUE))
cat("\nloglog\n")
r4$cpu.used
r4$summary.fixed[, "mean"]

## cauchits
set.seed(123)
p = inla.link.invcauchit(eta)
y = rbinom(n,  size = N, prob = p)
r5 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
        control.family = list(control.link = list(model = "cauchit")), control.predictor = list(compute=TRUE))
cat("\ncauchit\n")
r5$cpu.used
r5$summary.fixed[, "mean"]

## ccloglog
set.seed(123)
p = inla.link.invccloglog(eta)
y = rbinom(n,  size = N, prob = p)
r6 = inla(y ~ 1 + z,  data = data.frame(y, z), family = "binomial", Ntrials = rep(N, n),
        control.family = list(control.link = list(model = "ccloglog")), control.predictor = list(compute=TRUE))
cat("\nccloglog\n")
r6$cpu.used
r6$summary.fixed[, "mean"]
