library(INLA)

robeta <- function(eta, k1, k2, precision, debug = FALSE)
{
    stopifnot(k1 <= k2)
    g <- function(...) inla.link.invlogit(...)

    p <- cbind(1 - g(eta - k1), g(eta - k1) - g(eta - k2), g(eta - k2))

    if (debug) {
        ## verify probabilities for each sample
        if (all(abs(rowSums(p) - 1) < 1e-10)) {  # allow small numerical tolerance
            print("Success: probabilities for each sample sum to 1.")
        } else {
            stop("Error: at least one sample has invalid probabilities.")
        }
    }    
    eta <- as.vector(eta)
    n <- length(eta)
    group <- numeric(n)
    x <- numeric(n)
    for(i in 1:n) {
        group[i] <- sample(1:3, 1, prob = p[i, ])
        if (group[i] == 1) {
            x[i] <- 0
        } else if (group[i] == 3) {
            x[i] <- 1
        } else {
            mu <- g(eta[i])
            a <- mu * precision
            b <- -mu * precision + precision
            x[i] <- rbeta(1, a, b)
        }
    }

    if (debug) {
        ## check the observed and theoretical proportions of each outcome [zero, (0,1), and one, respectively]
        print(paste("Observed Proportions:", paste(c(mean(x == 0), mean(x > 0 & x < 1), mean(x == 1)), collapse = ", ")))
        print(paste("Theoretical Proportions:", paste(round(colMeans(p), 2), collapse = ", ")))
    }

    return (x)
}

n <- 10000
x <- rnorm(n, sd = 0.2)
eta <- 0 + x
k1 <- -2
k2 <- 2
prec <- 2
y <- robeta(eta, k1, k2, prec)

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "obeta",
          control.inla = list(cmin = 0), 
          verbose = TRUE,
          control.compute = list(cpo = TRUE))

summary(r)
