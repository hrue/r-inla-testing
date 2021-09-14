
n = 10
m = 35
N = n*m
y = rnorm(N)
idx = rep(1:n, m)
g = rep(1:m, each = n)

r = inla(y ~ -1 + f(idx, group = g,
                    control.group = list(
                        model = "besag",
                        scale.model = FALSE, 
                        graph = "graph-disconnected.dat")),
         data = data.frame(y, idx, g),
         verbose=T)

         
