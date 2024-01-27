source("./pgev.R")

INLA:::inla.my.update(b = T)
inla.setOption(verbose = !TRUE, num.threads = "1:1", safe = FALSE)

n <- 10000
x <- rnorm(n, sd = .5)
intercept <- runif(1)
beta.x <- runif(1, 0.5, 1.5)
eta <- intercept + beta.x * x

xi <- 0.3
p.intercept <- 1 - pgev(intercept, xi)
prob <- 1 - pgev(eta, xi)
size <- 1
y <- rbinom(n, size = size, prob = prob)

r <- inla(y ~ -1 + x,
          data = data.frame(y, x), 
          family = "binomial",
          Ntrials = size,
          control.inla = list(cmin = Inf, int.strategy = "eb"), 
          control.family = list(
              control.link =
                  list(model = "cgev",
                       hyper = list(tail = list(param = c(7, 0, 0.5)),
                                    intercept = list(initial = -1, param = c(0, 1))))))
summary(r)
print(c(n = n, intercept = intercept, p.intercept = p.intercept, beta.x = beta.x, xi = xi))
