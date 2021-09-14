n=10
m=n
A = matrix(sample(c(0,1), m*n, replace=T), m,n)
AA = as(A, "dgTMatrix")

z = rnorm(n)
y = z + 1
i = 1:n

lc = inla.make.lincombs(i=A, "(Intercept)" = 1:m)
print(names(lc))

r = inla(y ~ 1 + f(i), data = data.frame(y,i),
        lincomb = lc)

