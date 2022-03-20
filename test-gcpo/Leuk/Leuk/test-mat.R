xx <- read.table("OUT.new")

trim <- function(A) {
    idx <- c()
    for(i in 1:nrow(A)) {
        if (any(is.na(A[i, ])))
            idx <- c(idx, i)
    }
    A <- A[-idx, -idx, drop = FALSE]
    return (A)
}

neg.eig <- c()
for(i in 1:(dim(xx)[1]/25)) {
    istart <- 1 + (i-1)*25
    A <- xx[istart:(istart+24), ]
    AA <- trim(A)
    eig <- eigen(AA)$values
    if (any(eig <= 0)) {
        neg.eig <- c(neg.eig, eig[eig <= 0])
    }
}

