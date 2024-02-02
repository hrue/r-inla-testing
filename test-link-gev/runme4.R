n <- 300
x <- rnorm(n, sd = .5)
intercept <- runif(1)
beta.x <- runif(1, 0.5, 1.5)
eta <- intercept + beta.x * x

xi <- 0.1
p.intercept <- inla.link.invgev(intercept, tail = xi)
prob <- inla.link.invgev(eta, tail = xi)
size <- 2
y <- rbinom(n, size = size, prob = prob)

r <- inla(y ~ 1 + x,
          data = data.frame(y, x), 
          family = "binomial",
          Ntrials = size,
          control.inla = list(cmin = Inf, int.strategy = "eb"), 
          control.fixed = list(remove.names = "(Intercept)"),
          control.family = list(
              control.link =
                  list(model = "gev",
                       hyper = list(tail = list(param = c(7, 0, 0.5)),
                                    intercept = list(initial = 0, param = c(0, 1))))))

summary(r)
print(round(dig = 3, c(n = n, p.intercept = p.intercept, beta.x = beta.x, xi = xi)))
 

p.intercept <- 1 - inla.link.invgev(intercept, tail = xi)
prob <- 1 - inla.link.invgev(eta, tail = xi)
## to get the same data
y <- size - y

rc <- inla(y ~ -1 + x,
           data = data.frame(y, x), 
           family = "binomial",
           Ntrials = size,
           control.inla = list(cmin = Inf, int.strategy = "eb"), 
           control.family = list(
               control.link =
                   list(model = "cgev",
                        hyper = list(tail = list(param = c(7, 0, 0.5)),
                                     intercept = list(initial = 0, param = c(0, 1))))))
summary(rc)
print(round(dig = 3, c(n = n, p.intercept = p.intercept, beta.x = beta.x, xi = xi)))
