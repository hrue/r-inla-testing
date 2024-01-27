pgev = function(y, xi) {
    qalpha <- 0
    alpha <- 0.5
    beta <- 0.5
    sbeta <- 1

    l1.xi <- (-log(alpha))^(-xi)
    l2.xi <- (-log(1-beta/2))^(-xi)
    l3.xi <- (-log(beta/2))^(-xi)

    pfrechet = function(y) {
        aux1 <- (y - qalpha) / (sbeta * (l2.xi - l3.xi)^(-1)) + l1.xi
        return (exp(-pmax(0, aux1^(-1/xi))))
    }

    return (pfrechet(y))
}

INLA:::inla.my.update(b = T)
inla.setOption(verbose = !TRUE, num.threads = "1:1", safe = FALSE)

n <- 100
x <- rnorm(n, sd = .5)
intercept <- runif(1)
beta.x <- runif(1, 0.5, 1.5)
eta <- intercept + beta.x * x

xi <- 0.2
p.intercept <- pgev(intercept, xi)
prob <- pgev(eta, xi)
size <- 1
y <- rbinom(n, size = size, prob = prob)

r <- inla(y ~ -1 + x,
          data = data.frame(y, x), 
          family = "binomial",
          Ntrials = size,
          control.inla = list(cmin = Inf, int.strategy = "eb"), 
          control.family = list(
              control.link =
                  list(model = "gev",
                       hyper = list(tail = list(param = c(7, 0, 0.5)),
                                    intercept = list(initial = 0, param = c(0, 1))))))

summary(r)
print(round(dig = 3, c(n = n, p.intercept = p.intercept, beta.x = beta.x, xi = xi)))


p.intercept <- 1 - pgev(intercept, xi)
prob <- 1 - pgev(eta, xi)
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
