INLA:::inla.my.update()
inla.setOption(inla.mode = "experimental")
inla.setOption(num.threads = "4:1")
inla.setOption(inla.call = "inla.mkl.work")

n <- 10000
pcure <- 0.1
x <- scale(rnorm(n))
xx <- scale(rnorm(n))
eta <- 1 + 0.2 * x
eta.cure <- -1 + 0.2 * xx

run.exponential <- function() {
    y <- rexp(n, rate = exp(-eta))
    time.max <- quantile(y, 1-pcure)
    for(i in 1:n) {
        if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
            y[i] <- time.max
        }
    }
    
    event <- rep(1, n)
    event[y > time.max] <- 0
    Y <- inla.surv(y, event = event, cure = cbind(1, xx))
    r <- inla(Y ~ 1 + x,
              data = list(Y = Y, x = x),
              family = "exponentialsurv",
              control.family = list(link = list(model = "neglog")),
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.inla = list(cmin = 0.0), 
              verbose = TRUE,
              safe = FALSE)
    summary(r)
}

run.gamma <- function() {
    mu <- exp(eta)
    y <- rgamma(n, shape = mu, scale = 1)
    time.max <- quantile(y, 1-pcure)
    for(i in 1:n) {
        if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
            y[i] <- time.max
        }
    }
    event <- rep(1, n)
    event[y > time.max] <- 0
    Y <- inla.surv(y, event = event, cure = cbind(1, xx))
    r <- inla(Y ~ 1 + x,
              data = list(Y = Y, x = x),
              family = "gammasurv",
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.inla = list(cmin = 0.0, control.vb = list(emergency = 25)), 
              verbose = TRUE,
              safe = FALSE)
    summary(r)
}

run.gammajw <-  function() {
    eta <- 1 + x
    mu <- exp(eta)
    y <- rgamma(n, shape = mu, scale = 1)
    time.max <- quantile(y, 1-pcure)
    for(i in 1:n) {
        if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
            y[i] <- time.max
        }
    }
    event <- rep(1, n)
    event[y > time.max] <- 0
    Y <- inla.surv(y, event = event, cure = cbind(1, xx))
    r <- inla(Y ~ 1 + x,
              data = list(Y = Y, x = x),
              family = "gammajwsurv",
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.inla = list(cmin = 0.0, control.vb = list(emergency = 25)), 
              verbose = TRUE,
              safe = FALSE)
    summary(r)
}

run.weibull.0 <- function() {
    lambda <- exp(eta)
    alpha <- 1.2
    variant <- 0
    y = rweibull(n,
                 shape= alpha,
                 scale= if (variant == 0)
                            lambda^(-1/alpha)
                        else
                            1/lambda)

    time.max <- quantile(y, 1-pcure)
    for(i in 1:n) {
        if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
            y[i] <- time.max
        }
    }
    event <- rep(1, n)
    event[y > time.max] <- 0
    Y <- inla.surv(y, event = event, cure = cbind(1, xx))
    r <- inla(Y ~ 1 + x,
              data = list(Y = Y, x = x),
              family = "weibullsurv",
              control.family = list(variant = variant), 
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.inla = list(cmin = 0.0, control.vb = list(emergency = 25)), 
              verbose = TRUE,
              safe = FALSE)
    r <- inla.rerun(r)
    summary(r)
}

run.weibull.1 <- function() {
    lambda <- exp(eta)
    alpha <- 1.2
    variant <- 1
    y = rweibull(n,
                 shape= alpha,
                 scale= if (variant == 0)
                            lambda^(-1/alpha)
                        else
                            1/lambda)
    time.max <- quantile(y, 1-pcure)
    for(i in 1:n) {
        if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
            y[i] <- time.max
        }
    }
    event <- rep(1, n)
    event[y > time.max] <- 0
    Y <- inla.surv(y, event = event, cure = cbind(1, xx))
    r <- inla(Y ~ 1 + x,
              data = list(Y = Y, x = x),
              family = "weibullsurv",
              control.family = list(variant = variant), 
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.inla = list(cmin = 0.0, control.vb = list(emergency = 25)), 
              verbose = TRUE,
              safe = FALSE)
    r <- inla.rerun(r)
    summary(r)
}

run.lognormal <- function() {

    y <- rlnorm(n, meanlog = eta, sdlog = 1)
    time.max <- quantile(y, 1-pcure)
    for(i in 1:n) {
        if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
            y[i] <- time.max
        }
    }
    event <- rep(1, n)
    event[y > time.max] <- 0
    Y <- inla.surv(y, event = event, cure = cbind(1, xx))
    r <- inla(Y ~ 1 + x,
              data = list(Y = Y, x = x),
              family = "lognormalsurv",
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.inla = list(cmin = 0.0, control.vb = list(emergency = 25)), 
              verbose = TRUE,
              keep = T, 
              safe = FALSE)
    summary(r)
}


rloglogistic = function(n, lambda, alpha, variant=0)
{
    u = runif(n)
    if (variant == 0) {
        y = (lambda/(1.0/u - 1.0))^(1.0/alpha)
    } else if (variant == 1) {
        y = (1.0/(1.0/u -1.0))^(1.0/alpha) / lambda
    } else {
        stop("ERROR")
    }
}

run.loglogistic.0 <- function() {
    alpha = 2.1
    lambda = exp(eta)    
    y = rloglogistic(n, lambda = lambda, alpha = alpha, variant = 0)
    time.max <- quantile(y, 1-pcure)
    for(i in 1:n) {
        if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
            y[i] <- time.max
        }
    }
    event <- rep(1, n)
    event[y > time.max] <- 0
    Y <- inla.surv(y, event = event, cure = cbind(1, xx))
    r <- inla(Y ~ 1 + x,
              data = list(Y = Y, x = x),
              family = "loglogisticsurv",
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.family = list(variant = 0), 
              control.inla = list(cmin = 0.0, control.vb = list(emergency = 25)), 
              verbose = TRUE,
              safe = FALSE)
    summary(r)
}

run.loglogistic.1 <- function() {
    alpha = 2.1
    lambda = exp(eta)    
    y = rloglogistic(n, lambda = lambda, alpha = alpha, variant = 1)
    time.max <- quantile(y, 1-pcure)
    for(i in 1:n) {
        if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
            y[i] <- time.max
        }
    }
    event <- rep(1, n)
    event[y > time.max] <- 0
    Y <- inla.surv(y, event = event, cure = cbind(1, xx))
    r <- inla(Y ~ 1 + x,
              data = list(Y = Y, x = x),
              family = "loglogisticsurv",
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.family = list(variant = 1), 
              control.inla = list(cmin = 0.0, control.vb = list(emergency = 250)), 
              verbose = TRUE,
              safe = FALSE)
    summary(r)
}

lam_loglogistic = function(yq, alpha, q, variant = 0)
{
    if (variant == 0) {
        lambda = yq^alpha * (1/q-1)
    } else if (variant == 1) {
        lambda = 1/yq * (1/(1/q-1))^(1/alpha)
    } else
        stop("ERR")
    return (lambda)
}

run.qloglogistic.0 <- function() {
    alpha = 2.1
    yq = exp(eta)    
    q <- 0.5
    lambda = lam_loglogistic(yq, alpha, q, variant=0)
    y = rloglogistic(n,
                     lambda = lambda,
                     alpha = alpha,
                     variant = 0)
    time.max <- quantile(y, 1-pcure)
    for(i in 1:n) {
        if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
            y[i] <- time.max
        }
    }
    event <- rep(1, n)
    event[y > time.max] <- 0
    Y <- inla.surv(y, event = event, cure = cbind(1, xx))
    r <- inla(Y ~ 1 + x,
              data = list(Y = Y, x = x),
              family = "qloglogisticsurv",
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.family = list(variant = 0,
                                    control.link = list(quantile = q)), 
              control.inla = list(cmin = 0.0, control.vb = list(emergency = 250)), 
              verbose = TRUE,
              safe = FALSE)
    summary(r)
}

run.qloglogistic.1 <- function() {
    alpha = 2.1
    yq = exp(eta)    
    q <- 0.5
    lambda = lam_loglogistic(yq, alpha, q, variant=1)
    y = rloglogistic(n,
                     lambda = lambda,
                     alpha = alpha,
                     variant = 1)
    time.max <- quantile(y, 1-pcure)
    for(i in 1:n) {
        if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
            y[i] <- time.max
        }
    }
    event <- rep(1, n)
    event[y > time.max] <- 0
    Y <- inla.surv(y, event = event, cure = cbind(1, xx))
    r <- inla(Y ~ 1 + x,
              data = list(Y = Y, x = x),
              family = "qloglogisticsurv",
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.family = list(variant = 1,
                                    control.link = list(quantile = q)), 
              control.inla = list(cmin = 0.0, control.vb = list(emergency = 250)), 
              verbose = TRUE,
              safe = FALSE)
    summary(r)
}

run.gompertz <- function() {
    library(flexsurv)
    alpha <- 1
    mu <- exp(eta)
    y <- rgompertz(n, rate = mu, shape = alpha)

    time.max <- quantile(y, 1-pcure)
    for(i in 1:n) {
        if (runif(1) < 1/(1+exp(-eta.cure[i]))) {
            y[i] <- time.max
        }
    }
    event <- rep(1, n)
    event[y > time.max] <- 0
    Y <- inla.surv(y, event = event, cure = cbind(1, xx))
    r <- inla(Y~ 1 + x,
              data = list(Y = Y, x = x),
              family = "gompertzsurv",
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.inla = list(cmin = 0.0, control.vb = list(emergency = 25)), 
              verbose = TRUE,
              safe = FALSE)
    summary(r)
}

