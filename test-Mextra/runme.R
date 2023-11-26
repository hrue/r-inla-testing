n <- 10
if (FALSE) {
    idx <- vector('list', n)
    for(i in 1:n) {
        if (i > 1 && i < n) {
            idx[[i]] <- c(i, i-1, i+1)
        } else {
            idx[[i]] <- i
        }
    }
}

idx <- sparseMatrix(i = 1, j = 1, x = 0, dims = c(n, 3))

for(i in 1:n) {
    if (i > 1 && i < n) {
        idx[i, ]<- c(i, i-1, i+1)
    } else {
        idx[i, 1] <- i
    }
}


r <- inla(y ~ f(idx),
          data = list(y = rnorm(n), idx = idx),
          family = "stdnormal",
          verbose = T, debug = T, safe = F)


