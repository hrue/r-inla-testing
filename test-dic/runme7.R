n <- 50
u <- rnorm(n, 1)
x <- rnorm(n, 1)
y <- 1 + x + u + rnorm(n, 1)

## the real model is y ~ 1 + x, with variance = 2, but we estimate it adding an iid model, u, to
## it. this example shows how to 'get' the DIC/CPO when using the wrong model.

## true
r.true <- inla(y ~ 1 + x,
               data = data.frame(y, x),
               control.family = list(hyper = list(prec = list(initial = log(1/2),
                                                              fixed = TRUE))),
               control.compute = list(dic = TRUE, cpo = TRUE))

## fitting another model
r <- inla(y ~ 1 + x + f(idx, model = "iid",
                        hyper = list(prec = list(initial = log(1),
                                                 fixed = TRUE))),
          data = data.frame(y, x, idx = 1:n), 
          control.family = list(hyper = list(prec = list(initial = log(1),
                                                              fixed = TRUE))),
          control.compute = list(config = TRUE, cpo = TRUE))


## there is a vignette on how 'inla.posterior.sample.eval'-works
fun.deviance <- function(y) {
    return (sum(-2*dnorm(y, mean = Predictor - idx, sd = sqrt(2), log = TRUE)))
}

xx <- inla.posterior.sample(10000, r)
e.dev <- mean(inla.posterior.sample.eval(fun.deviance, xx, y = y))
dev.e <- sum(-2*dnorm(y, mean = r$summary.linear.predictor$mean - r$summary.random$idx$mean,
                      sd = sqrt(2), log = TRUE))
p.eff <- e.dev - dev.e
dic <- p.eff + sum(e.dev)

print(round(dig = 3,
            cbind(INLA = c(r.true$dic$dic,  r.true$dic$p.eff),
                  EST = c(dic, p.eff))))

