all.files = list.files(pattern='[.]dat$')

for(file in all.files) {

    print(file)

    file.xy = paste(file, ".C", sep="")
    file.x = paste(file, ".P", sep="")
    if (file.exists(file.xy) && file.exists(file.x)) {
        xy = try(read.table(file.xy),  silent=TRUE)
        x = try(read.table(file.x),  silent=TRUE)
        if (!inherits(xy, "try-error") && !inherits(xy, "try-error")) {
            plot(xy)
            lines(xy)
            points(x, pch=19)
            title(paste(file,
                        "s", format(x[1], digits=3),
                        "k", format(x[2], digits=3), 
                        "p", format(x[3], digits=3),
                        "l", format(x[4], digits=3),
                        "r",  format(x[3]/x[4], digits=3)))
            dev.print(pdf, file=paste(file, ".pdf", sep=""))
        }
    }
}

    
