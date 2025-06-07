n = 10000
s = 0.1
x = rnorm(n,sd=s)
z = rnorm(n,sd=s)
y = numeric(n)
## I want to be able to look at this one
p = numeric(n)
N = 1+rpois(n, lambda = 50)
alpha = 0.3

for(i in 1:n) {
    eta = x[i] + z[i]
    m = exp(eta)
    ## OOPS: q = 1-p
    p[i] = 1-(m/(1+m))^alpha
    prob = exp(eta)/(1+exp(eta))
    xx = 0:N[i]
    d = dbinom(xx, size = N[i], prob = prob)
    len = length(d)
    d[1] = p[i] + (1-p[i])*d[1]
    d[2:len] = (1-p[i])*d[2:len]
    y[i] = sample(xx, 1, prob=d)
}

i = 1:n
formula = y ~ f(i, model="iid") + z
data = data.frame(y=y, x=x, z=z, N=N, i=i)

r = inla(formula, family = "zeroinflatedbinomial2", data=data, Ntrials=N, verbose=TRUE)
summary(r)



    
         
