n <- 25
x <- rnorm(n)
s <- 0.1
y <- 1 + x + rnorm(n, sd = s)

jp = function(theta, theta.desc = NULL) {
    INLA:::inla.require("dplyr")
    if (!is.null(theta.desc)) {
        for(i in seq_along(theta.desc))
            print(paste0("    theta[", i, "]=", theta.desc[i]))
    }
    param.1 = c(100, 10) 
    param.2 = c(1, 10)  
    return((dgamma(exp(theta[1]), shape = param.1[1], rate = param.1[2], log=TRUE) +
            theta[1] +
            dnorm(theta[2], mean = param.2[1], sd = 1/sqrt(param.2[2]), log = TRUE)))
}

jpr = inla.jp.define(jp) 

r <- inla(y ~ 1 +
              x +
              f(idx, w, model = "iid",
                hyper = list(prec = list(
                                 initial = 0,
                                 fixed = TRUE))) +
              f(iidx, w, copy = "idx", fixed = FALSE), 
          control.family = list(hyper = list(prec = list(
                                                 prior = "loggamma",
                                                 param = c(100, 10)))), 
          data = data.frame(y,
                            x,
                            idx = rep(1, n),
                            w = rep(0, n),
                            iidx = rep(1, n)), 
          verbose = TRUE)

rr <- inla(y ~ 1 +
              x +
              f(idx, w, model = "iid",
                hyper = list(prec = list(
                                 initial = 0,
                                 fixed = TRUE))) +
              f(iidx, w, copy = "idx", fixed = FALSE), 
          control.family = list(hyper = list(prec = list(
                                                 prior = "loggamma",
                                                 param = c(100, 10)))), 
          control.expert = list(jp = jpr),
          data = data.frame(y,
                            x,
                            idx = rep(1, n),
                            w = rep(0, n),
                            iidx = rep(1, n)), 
          verbose = TRUE)
summary(r)
summary(rr)

r$mlik - rr$mlik

