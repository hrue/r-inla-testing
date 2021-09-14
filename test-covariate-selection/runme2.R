n = 30
z = rnorm(n)
zz = rnorm(n)
noise = rnorm(n, sd = 10)

y = 1 + z + zz  + noise

formula0 = y ~ 1
formula1 = y ~ 1 + z.tmp

r00 = inla(formula0, data = data.frame(y), family = "gaussian")$mlik[[1]]

z.tmp = z
r10 = inla(formula1, data = data.frame(y, z.tmp), family = "gaussian")$mlik[[1]]

z.tmp = zz
r01 = inla(formula1, data = data.frame(y, z.tmp), family = "gaussian")$mlik[[1]]

z.tmp = z + zz
r11 = inla(formula1, data = data.frame(y, z.tmp), family = "gaussian")$mlik[[1]]

mlik = c(r00, r10, r01, r11)
mlik = mlik - max(mlik)
p = exp(mlik) / sum(exp(mlik))

prob.z = p[2] + p[4]
prob.zz = p[3] + p[4]

print(c(prob.z, prob.zz))
