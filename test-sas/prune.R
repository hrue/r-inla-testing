stop("does this work?")

fd = file("sas-prior-table-2000.dat", "rb")
x = readBin(fd, integer(), 3L)
x = as.integer(x)
y = readBin(fd, double(), x[1] + x[2] + x[1]*x[2])
close(fd)

for(fac in 55L) {
    print(fac)
    xx = x
    xx[1:2] = x[1:2] %/% fac
    xx[3] = xx[1] * xx[2]
    yy = numeric(xx[1] + xx[2] + xx[1]*xx[2])

    yy[1:xx[1]] = y[(1:xx[1] -1L)*fac +1L]
    yy[xx[1] + 1:xx[2]] = y[x[1] + (1:xx[2] - 1L)*fac +1L]

    y.off = x[1] + x[2]
    yy.off = xx[1] + xx[2]

    for(ii in 1:xx[1]) {
        jj = 1:xx[2]
        kk = ii + (jj-1) * xx[1]
        i = (ii-1)*fac + 1
        j = (jj-1)*fac + 1
        k = i + (j-1) * x[1]
        print(ii)
        print(jj)
        print(kk)
        print(i)
        print(j)
        print(k)
        print(y[y.off + k])
        yy[yy.off + kk] = y[y.off + k]
    }

    fd = file(paste("sas-prior-table-", as.integer(xx[1]), ".dat", sep=""), "wb")
    writeBin(as.integer(xx), fd)
    writeBin(yy, fd)
    close(fd)
}
