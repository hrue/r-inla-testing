smooth.sas.prior = function(file = "sas-prior-table.dat", ...)
{
    do.smooth.matrix = function(A, edge.check = FALSE, ...)
    {
        nx = nrow(A)
        ny = ncol(A)

        smooth.vector = function(vec, edge.check = FALSE, ...)
        {
            x = which(!is.na(vec))
            if (length(x) > 3) {
                if (edge.check) {
                    ## this is for the point variable, where we do not
                    ## want to smooth over (skew=0, { kappa < 3})
                    ## where the point goes from 0 to high.
                    y = vec[x]
                    y.diff = abs(diff(y))
                    if (max(y.diff) > 0.5) {
                        ## smooth in two steps
                        idx.max = which.max(y.diff)

                        xx = x[1:idx.max]
                        yy = y[1:idx.max]
                        vec[xx] = supsmu(xx, yy , ...)$y

                        xx = x[(idx.max+1):length(x)]
                        yy = y[(idx.max+1):length(x)]
                        vec[xx] = supsmu(xx, yy , ...)$y

                    } else {
                        vec[x] = supsmu(x, vec[x], ...)$y
                    }
                } else {
                    vec[x] = supsmu(x, vec[x], ...)$y
                }
            }
            return (vec)
        }

        h = 0
        Asmooth = matrix(0, nrow(A), ncol(A))
        for(krow in (-h:h)) {
            for(kcol in (-h:h)) {
                Atmp = A
                for(i in 1:nrow(A)) {
                    idx = ((0:(ncol(A)-1) + kcol + ncol(A)) %% ncol(A)) + 1
                    Atmp[i, idx] = A[i, 1:ncol(A)]
                }
                for(j in 1:ncol(A)) {
                    idx = ((0:(nrow(A)-1) + krow + nrow(A)) %% nrow(A)) + 1
                    Atmp[idx, j] = A[1:nrow(A), j]
                }
                A.by.row = matrix(apply(Atmp, 1, smooth.vector, ...),
                        nrow(A), ncol(A),  byrow=TRUE)
                A.by.col = apply(Atmp, 2, smooth.vector, edge.check = edge.check, ...)
                Asmooth = Asmooth + (A.by.col + A.by.row)/2.0
            }
        }
        Asmooth = Asmooth / (2*h+1)^2
        return (Asmooth)
    }

    fd = file(file, "rb")
    x = readBin(fd, integer(), 3L)
    nx = x[1]
    ny = x[2]
    nz = x[3]

    skew = readBin(fd, double(), nx)
    kurt = readBin(fd, double(), ny)
    level = matrix(readBin(fd, double(), nz), nx, ny)
    len = matrix(readBin(fd, double(), nz), nx, ny)
    point = matrix(readBin(fd, double(), nz), nx, ny)
    close(fd)
    
    cat("smooth level...\n")
    level = do.smooth.matrix(level, ...)

    cat("smooth len...\n")
    len = do.smooth.matrix(len, ...)

    cat("smooth point/len...\n")
    point.len = do.smooth.matrix(point/len, edge.check=TRUE, ...)
    cat("recreate point as point/len * len...\n")
    point = point.len * len

    fd = file(paste(file, "-SMOOTH", sep=""), "wb")
    writeBin(as.integer(c(nx, ny, nz)), fd)
    writeBin(skew, fd)
    writeBin(kurt, fd)
    writeBin(c(level), fd)
    writeBin(c(len), fd)
    writeBin(c(point), fd)
    close(fd)

}
