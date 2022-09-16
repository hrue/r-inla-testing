INLA:::inla.my.update()
inla.setOption(num.threads = "1:1",
               inla.mode = "experimental",
               inla.call = "inla.mkl.work")
n <- 10
sx <- 0.1
sy <- 10
if (!exists("y")) {
    y <- 0+rnorm(n, sd = sx) + rnorm(n, sd = sy)
    groups <- vector('list', n)
    K <- 3
    for(i in 1:n) {
        groups[[i]] <- c(i, sample(setdiff(1:n, i), K, replace = FALSE))
    }
}
r <- inla(y ~ 1 + f(idx, model = "iid",
                    hyper = list(
                        prec = list(
                            initial = log(1/sx^2),
                            fixed = TRUE))), 
          data = data.frame(y, idx = 1:n),
          safe = FALSE, 
          control.family = list(hyper = list(prec = list(initial = log(1/sy^2),
                                                         fixed = TRUE))),
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     verbose = !TRUE, 
                                                     groups = groups
                                                     )),
          verbose = TRUE)

gcpo <- c()
for(i in 1:n) {
    yy <- y
    g <- groups[[i]]
    yy[g] <- NA
    rr <- inla(y ~ 1 + f(idx, model = "iid",
                         hyper = list(
                             prec = list(
                                 initial = log(1/sx^2),
                                 fixed = TRUE))), 
               data = data.frame(y = yy, idx = 1:n),
               safe = FALSE, 
               control.family = list(hyper = list(
                                         prec = list(
                                             initial = log(1/sy^2),
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
    gcpo <- c(gcpo, dnorm(y[i], mean =  mm,  sd = sqrt(sd^2 + sy^2)))
}
