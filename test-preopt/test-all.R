n <- 30
ns <- 10
x <- rnorm(ns, sd = 1)
xx <- rnorm(ns, sd = 1)
xxx <- rnorm(ns, sd = 1)

m <- n+1
while(TRUE) {
    A <- matrix(rnorm(n*m) * rbinom(n*m, prob = 5/n, size = 1), m, n)
    ##diag(A) <- 1
    if (all(rowSums(A != 0) > 0)) break
}
    
    

id.x <- sample(1:ns, n, replace = TRUE)
id.xx <- sample(1:ns, n, replace = TRUE)
id.xxx <- sample(1:ns, n, replace = TRUE)

s <- 0.1
eta <- 1 + x[id.x] + xx[id.xx] + xxx[id.xxx]
y <- A %*% eta + rnorm(m, sd = s)

INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work", num.threads = "1:1")
##inla.setOption(inla.call = "inla.valgrind", num.threads = "1:1")

r <- rep(list(list()), 3)

while(TRUE) {
    TF <- sample(c(TRUE, FALSE), 3,  replace = TRUE)

    mo <- c("classic", "twostage", "experimental")
    for (i in seq_along(mo)) {
        r[[i]] <- inla(y ~ 1 +
                           f(id.x, model = "iid", values = 1:ns,
                             hyper = list(prec = list(param = c(10, 10), fixed = TF[1], initial = 0))) +
                           f(id.xx, model = "iid", values = 1:ns, 
                             hyper = list(prec = list(param = c(10, 10), fixed = TF[2], initial = 0))) +
                           f(id.xxx, model = "iid", values = 1:ns, 
                             hyper = list(prec = list(param = c(10, 10), fixed = TF[3], initial = 0))),
                       data = list(y = y, id.x = id.x, id.xx = id.xx, id.xxx = id.xxx, A = A), 
                       control.predictor = list(A = A), 
                       control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))),
                       control.compute = list(dic = T, cpo = T, waic = T, po = T, config = T),
                       inla.mode = mo[i],
                       verbose = (i == -333))
        r[[i]] <- inla.rerun(r[[i]])
    }

    print(round(dig = 2,
                c(as.numeric(TF),
                  as.numeric(c(r[[1]]$mlik[1, 1] - r[[2]]$mlik[1, 1], 
                               r[[1]]$mlik[1, 1] - r[[3]]$mlik[1, 1])),
                  cor(r[[1]]$cpo$cpo, r[[2]]$cpo$cpo),
                  cor(r[[1]]$cpo$cpo, r[[3]]$cpo$cpo),
                  cor(r[[1]]$cpo$pit, r[[2]]$cpo$pit),
                  cor(r[[1]]$cpo$pit, r[[3]]$cpo$pit),
                  cor(r[[1]]$po$po, r[[2]]$po$po),
                  cor(r[[1]]$po$po, r[[3]]$po$po),
                  cor(r[[1]]$dic$local.dic, r[[2]]$dic$local.dic), 
                  cor(r[[1]]$dic$local.dic, r[[3]]$dic$local.dic))))
}

