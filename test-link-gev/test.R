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

if (FALSE) {
    xis <- seq(0.0001, 0.49, len = 1000)
    plot(xis, pgev(0.2, xis))
    inla.dev.new()
}


INLA:::inla.my.update(b = T)
inla.setOption(verbose = !TRUE, num.threads = "1:1", safe = FALSE)

n <- 25
x <- rnorm(n, sd = .0005)
intercept <- runif(1)
beta.x <- 0
eta <- intercept + beta.x * x

xi <- 0.3
p.intercept <- pgev(intercept, xi)
prob <- pgev(eta, xi)
size <- 1
y <- rbinom(n, size = size, prob = prob)

r <- inla(y ~ 1,
          data = data.frame(y, x), 
          family = "binomial",
          Ntrials = size,
          control.fixed = list(prec = 1, prec.intercept = 1), 
          control.inla = list(cmin = Inf, int.strategy = "ccd"), 
          control.family = list(
              control.link =
                  list(model = "gev",
                       hyper = list(tail = list(param = c(2, 0, 0.5)),
                                    intercept = list(initial = Inf, param = c(0, 1))))))
rr <- inla(y ~ 1,
          data = data.frame(y, x), 
          family = "binomial",
          Ntrials = size,
          control.fixed = list(prec = 1, prec.intercept = 10000000), 
          control.inla = list(cmin = Inf, int.strategy = "ccd"), 
          control.family = list(
              control.link =
                  list(model = "gev",
                       hyper = list(tail = list(param = c(2, 0, 0.5)),
                                    intercept = list(initial = 0, param = c(0, 1))))))

summary(r)
print(c(n = n, intercept = intercept, p.intercept = p.intercept, beta.x = beta.x, xi = xi))
