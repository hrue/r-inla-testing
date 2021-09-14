n = 1000
x = seq(-5, 5, len=n)
y = dnorm(x)
m = cbind(x = x,  y = y)
m.right = cbind(x = x[x <= 0],  y = 2*y[x <= 0])
m.left = cbind(x = x[x >= 0],  y = 2*y[x >= 0])

p = c(0.025, 0.05, 0.90, 0.95)
print(inla.hpdmarginal(p, m))
print(inla.hpdmarginal(p, m.left))
print(inla.hpdmarginal(p, m.right))

      
