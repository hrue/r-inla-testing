n <- 100
treat <- as.factor(sample(c("treatment", "control"), n, replace = TRUE))
time <- as.factor(sample(c("baseline", paste0("t", 1:4), "endpoint"), n, replace = TRUE))

y <- rnorm(n)
r <- inla(y ~ treat + time + treat * time,
          data = data.frame(y, treat, time))

## these are the names to use for defining lincombs
rownames(r$summary.fixed)

## here is an example
lc <- inla.make.lincomb("timet2" = 1, "treattreatment:timeendpoint" = -1)
rr <- inla(y ~ treat + time + treat * time,
           data = data.frame(y, treat, time),
           lincomb = lc)

## with such simple contrast, its almost easier to doit manually, like
rrr <- inla(y ~ treat + time + treat * time,
           data = data.frame(y, treat, time),
           control.fixed = list(correlation.matrix = TRUE))
S <- rrr$misc$lincomb.derived.covariance.matrix
nf <- nrow(S)
w <- numeric(nf)
w[which(rownames(S) %in% "timet2")] <- 1
w[which(rownames(S) %in% "treattreatment:timeendpoint")] <- -1
v <- t(w) %*% S %*% w
m <- rrr$summary.fixed["timet2", "mean"] -
    rrr$summary.fixed["treattreatment:timeendpoint", "mean"]

## the results will not match in all digits as they are computed differently (like S above is
## the covariance where the hyperpar is integrated out, etc...), where the 'lc' is most accurate
print(c(m, sqrt(v)))
print(rr$summary.lincomb.derived[,c("mean","sd")])


    


                   
