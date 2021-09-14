n = 1000
x = runif(n)
y = sqrt(sqrt(x)) + rnorm(n, sd=0.001)

first.time = TRUE
for(n in seq(10,n, by = 50)) {
    print(n)
    r = inla(y ~ f(inla.group(x,n=n,method="quantile"), model="rw2", constr=F)-1,
            data=data.frame(x,y))
    f = r$summary.random[[1]]
    if (first.time)
        plot(f$ID, f$mean, type="l")
    else
        lines(f$ID, f$mean)

    first.time=FALSE
}



