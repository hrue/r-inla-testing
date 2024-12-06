nm <- 100
n <- 100
Y <- matrix(rnorm(nm*n), nm, n)

tref.1 <- Sys.time()
wd <- "model.files"
unlink(wd, recursive = TRUE)

for(i in 1:nm) {
    y <- Y[i, ]
    inla(y ~ 1, data = data.frame(y),
         working.directory = wd, 
         inla.call = "", keep = TRUE)
}
print(Sys.time() - tref.1)
r <- INLA:::inla.run.many(wd, num.threads = "1:1", cleanup = TRUE)
tref.1 <- Sys.time() - tref.1

tref.2 <- Sys.time()
rr <- rep(list(list()), nm)
for(i in 1:nm) {
    y <- Y[i, ]
    rr[[i]] <- inla(y ~ 1, data = data.frame(y))
}
tref.2 <- Sys.time() - tref.2

s <- ss <- 0
for(i in 1:nm) {
    s <- s + r[[i]]$mlik[1]
    ss <- ss + rr[[i]]$mlik[1]
}

print(c(s, ss, s-ss))
print(tref.1)
print(tref.2)
