
fib.define = function() {
    n.cache= 2
    fib.cache = c(1, 1)
    
    fib.func = function(n) {
        if (n <= n.cache) {
            print(paste("reuse", n))
            return (fib.cache[n])
        } else {
            for(nn in (n.cache + 1):n) {
                print(paste("cache", nn))
                fib.cache[nn] <<- fib.cache[nn-1] + fib.cache[nn-2]
            }
            print(paste("reuse", n))
            n.cache <<- n
            return (fib.cache[n])
        }
    }

    return (fib.func)
}


fib.func = function(n) {
    if (n <= cache$n) {
        print(paste("reuse", n))
        return (cache$fib[n])
        } else {
            for(nn in (cache$n + 1):n) {
                print(paste("cache", nn))
                cache$fib[nn] <<- cache$fib[nn-1] + cache$fib[nn-2]
            }
            print(paste("reuse", n))
            cache$n <<- n
            return (cache$fib[n])
        }
}
cache = list(n=2, fib=c(1, 1))
env = new.env()
assign("cache", cache, env)
environment(fib.func) = env
sa            

    
