
prob.interval <- function(y.low, y.high, size, mu) {

    s0 <- dnbinom(y.low, size = size, mu = mu)
    s <- s0
    p <- size/(size+mu)
    for(y in (y.low+1):y.high) {
        s0 <- s0 * (y+size-1) / y * (1-p)
        s <- s + s0
    }
    return (s)
}


mu <- runif(1)
size <- rpois(1, 2)
prob <- size/(size + mu)

y.low <- 4
y.high <- 6

##print(cbind(y.low:y.high, dnbinom(y.low:y.high, size = size, mu = mu)))

print(c(sum(dnbinom(y.low:y.high, size = size, mu = mu)),
        (pnbinom(y.high, size = size, mu = mu) - pnbinom(y.low-1, size = size, mu = mu)),
        prob.interval(y.low, y.high, size, mu)))

        
