nm <- 50
n <- 10

tref <- Sys.time()

wd <- "model.files"
dir.create(wd, recursive = TRUE)
inla.setOption(working.directory = wd)
treff <- Sys.time()
##Rprof()
for(i in 1:nm) {
    inla(y ~ 1, data = data.frame(y = rnorm(n)), inla.call = "", keep = TRUE)
}
##Rprof(NULL)

##summaryRprof()
print(Sys.time() - treff)
stop("XXXXXXXXX")
system2("inla.mkl.work", args = paste(c(" -t10:1 ", dir(wd, full.names = TRUE))))


res <- list(list())
i <- 1
for(d in dir(wd, full.names = TRUE)) {
    res[[i]] <- inla.collect.results(d)
    i <- i + 1
}

print(Sys.time()-tref)

tref <- Sys.time()
inla.setOption(working.directory = NULL)
for(i in 1:nm) {
    r[[i]] <- inla(y ~ 1, data = data.frame(y = rnorm(n)))
}
print(Sys.time()-tref)

