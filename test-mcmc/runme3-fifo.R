## in this order....
fifo.put = fifo("inla-mcmc-fifo-get", "wb", blocking=TRUE)
fifo.get = fifo("inla-mcmc-fifo-put", "rb", blocking=TRUE)

fifo.put.data = fifo("inla-mcmc-fifo-get-data", "wb", blocking=TRUE)
fifo.get.data = fifo("inla-mcmc-fifo-put-data", "rb", blocking=TRUE)

# N = dim(x) + dim(theta), NData is the dimension of the linear
# predictor.
N = 13
NData = 10

while(TRUE) {
    x = readBin(fifo.get, what = numeric(), n = N)
    stopifnot(length(x) == N)
    print(x)

    ## modify x
    print("modify x ...")
    system("sleep 1")
    ##

    writeBin(x, fifo.put)

    y = readBin(fifo.get.data, what = numeric(), n = NData)
    stopifnot(length(y) == NData)
    print(y)

    ## modify y
    print("modify y ...")
    system("sleep 1")
    ##

    writeBin(y, fifo.put.data)
}

