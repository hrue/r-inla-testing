## enable this first ime
##generate.data <- TRUE

if (generate.data) {
    generate.data <- FALSE
    n <- 300000
    xi.range <- c(0, 1)
    xi <- 0.7
    xi.intern <- inla.models()$likelihood$egp$hyper$theta1$to.theta(xi, interval=xi.range)
    alpha <- 0.9
    kappa <- 1.1
    kappa.intern <- inla.models()$likelihood$egp$hyper$theta2$to.theta(kappa)
    a <- ((1-alpha^(1/kappa))^(-xi) -1)
    x <- rnorm(n, sd = 0.3)
    eta <- 0.9 + 1.1 * x
    q <- exp(eta)
    sigma <- xi * q / a
    y <- - (sigma / xi) * (1- (1-runif(n)^(1/kappa))^(-xi))
}

r <- inla(y ~ 1 + x,
          family = "egp",
          control.family = list(
              control.link = list(quantile = alpha),
              hyper = list(tail = list(
                               initial = xi.intern,
                               fixed = FALSE, 
                               prior = "pc.egptail",
                               param = c(225, xi.range)), 
                           shape = list(
                               initial = kappa.intern, 
                               fixed = !TRUE, 
                               prior = "loggamma",
                               param = c(100, 100)))), 
          data = data.frame(y, x),
          num.threads = "8:2", 
          ##control.inla = list(verbose = F, tolerance.step = 1e-6), 
          verbose = TRUE)
print(summary(r))
