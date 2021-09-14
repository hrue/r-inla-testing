n = 5000
s = 0.1
x = rnorm(n,sd=s)
z = rnorm(n,sd=s)
y = numeric(n)
## I want to be able to look at this one
p = numeric(n)
E = 1+runif(n)
alpha = 0.5

for(i in 1:n)
{
    eta = x[i] + z[i]
    m = E[i]*exp(eta)
    ## OOPS: q = 1-p
    p[i] = 1-(m/(1+m))^alpha
    x.max = floor(m + sqrt(m)*7)
    xx = 0:x.max
    d = dpois(xx, lambda = m)
    len = length(d)
    d[1] = (1-p[i])*d[1] + p[i]
    d[2:len] = (1-p[i])*d[2:len]
    y[i] = sample(xx, 1, prob=d)
}

i = 1:n
formula = y ~ f(i, model="iid") + z
data = data.frame(y=y, x=x, z=z, E=E, i=i)

r = inla(formula, family = "zeroinflatedpoisson2", data=data, E=E, verbose=TRUE,
        control.inla=list(tolerance=1e-4))


    
         
