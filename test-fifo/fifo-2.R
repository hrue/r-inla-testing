N=10
fd1 = fifo("fifofile.1", "rb", blocking=TRUE)
fd2 = fifo("fifofile.2", "wb", blocking=TRUE)

while (TRUE) {

    print("R: read...")
    i = readBin(fd1, what = integer(), n = N)
    stopifnot(length(i) == N)
    print(paste(c("R: read i = ", i)))

    i = i + 1L
    print(paste(c("R: increase i = ", i)))

    print("R: write")
    writeBin(as.integer(i), fd2)
    print("R: write done")
}
