INLA:::inla.my.update()
inla.setOption(num.threads = "1:1",
               inla.mode = "experimental",
               inla.call = "inla.mkl.work")
n <- 20
x <- sort(rnorm(n)) ##+ cumsum(runif(n, max = 0.1))
s <- .01
y <- x + rnorm(n, sd = s)

K <- 3 ## level-sets

if (TRUE) {
    groups <- vector('list', n)
    if (FALSE) {
        for(i in 1:n) {
            if (i == 1) {
                groups[[i]] <- c(1, 2, 3)
            } else {
                if (K > 0) {
                    groups[[i]] <- c(1:K, i)
                } else {
                    groups[[i]] <- i
                }
            }
        }
    } else {
        for(i in 1:n) {
            groups[[i]] <- c(i, sample(setdiff(1:n, i), K-1, replace = FALSE))
        }
    }
} else {
    ## use automatic groups
    groups <- NULL
}
r <- inla(y ~ 1 + x, 
          data = data.frame(y, x, idx = 1:n),
          safe = FALSE, 
          control.fixed = list(prec.intercept = 0, prec = 0), 
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     verbose = !TRUE, 
                                                     epsilon = 0.005, 
                                                     groups = groups, 
                                                     num.level.sets = K
                                                     )),
          verbose = TRUE)

if (is.null(groups)) {
    groups <- lapply(r$gcpo$groups, function(x) x$idx)
}

gcpo <- c()
for(i in 1:n) {
    yy <- y
    g <- groups[[i]]
    yy[g] <- NA
    rr <- inla(y ~ 1 + x, 
               data = data.frame(y = yy, x, idx = 1:n),
               safe = FALSE, 
               control.fixed = list(prec.intercept = 0, prec = 0), 
               control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                              fixed = TRUE))))

    mm <- rr$summary.linear.predictor$mean[i]
    ss <- rr$summary.linear.predictor$sd[i]
    print(round(dig = 5,
                c(idx = i,
                  gcpo.mean = r$gcpo$mean[i],
                  mean = rr$summary.linear.predictor$mean[i],
                  err.mean = r$gcpo$mean[i] - rr$summary.linear.predictor$mean[i], 
                  gcpo.sd = r$gcpo$sd[i],
                  sd = rr$summary.linear.predictor$sd[i],
                  err.sd = r$gcpo$sd[i] / rr$summary.linear.predictor$sd[i] -1)))

    m <- rr$summary.linear.predictor$mean[i]
    sd <- rr$summary.linear.predictor$sd[i]
    gcpo <- c(gcpo, dnorm(y[i], mean =  mm,  sd = sqrt(sd^2 + s^2)))
}

plot(gcpo, r$gcpo$gcpo, pch = 19, xlab = "manual", ylab = "gcpo")
abline(a = 0, b = 1)
