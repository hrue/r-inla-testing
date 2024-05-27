INLA:::inla.my.update(b = T)
inla.setOption(num.threads = 4)

X=read.table("X.dat", header=T)
Y=read.table("Y.dat", header=T)

ss.list <- list(c(1, 3, 5), c(2, 4, 6))
for (ss in seq_along(ss.list)) {
    cols <- ss.list[[ss]]
    z <- unlist(X[, cols])
    m <- mean(z, na.rm = TRUE)
    s <- sd(z, na.rm = TRUE)
    X[, cols] <- (z-m)/s
}

X <- cbind(1, X[, 1], X[1, 2],   1, X[, 3], X[, 4],   1, X[, 4], X[, 5])
r <- inla(inla.mdata(Y, X) ~ 1,
          family = "occupancy",
          data = list(Y = Y, X = X),
          safe = FALSE,
          verbose = TRUE,
          control.fixed = list(prec.intercept = 1), 
          control.inla = list(cmin = 0.0))
summary(r)
cbind(beta, r$summary.hyperpar[, "mean"])
