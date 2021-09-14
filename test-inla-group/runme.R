
n=20
y = sin((1:n)/5) + rnorm(n, sd=1)


first.time = TRUE
for(k in 2^(0:5)) {
    print(k)
    values = seq(1,n,length=k*n-(k-1))
    N = length(values)
    x = values
    Y = numeric(N)
    Y[] = NA
    for(i in 1:n)
        Y[which( i  == values)] = y[i]

    formula = Y ~ f(values, model="rw1", values=values, param=c(1,0.01),
            constr=FALSE) -1
    result = inla(formula, data = data.frame(x,Y,values),verbose=T,keep=T)

    r = result$summary.random[[1]]
    if(first.time)
        plot(r$ID, r$mean, type="l")
    else
        lines(r$ID, r$mean)

    first.time=FALSE
}

    

