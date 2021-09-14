fifo.put = fifo("inla-mcmc-fifo-get", "wb", blocking=TRUE)
fifo.get = fifo("inla-mcmc-fifo-put", "rb", blocking=TRUE)

# N = dim(x) + dim(theta)
N = 13

while(TRUE) {
    x = readBin(fifo.get, what = numeric(), n = N)
    stopifnot(length(x) == N)
    print(x)
    ## modify x
    print("modify x ...")
    ##system("sleep 1")
    ##
    writeBin(x, fifo.put)
}

