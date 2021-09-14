n = 30
z = rnorm(n)
zz = rnorm(n)
noise = rnorm(n, sd = 10)

y = 1 + z + zz  + noise

data = data.frame(y, z, zz)
formula00 = y ~ 1
formula10 = y ~ 1 + z
formula01 = y ~ 1 + zz
formula11 = y ~ 1 + z + zz

r00 = inla(formula00, data = data, family = "gaussian")$mlik[[1]]
r01 = inla(formula01, data = data, family = "gaussian")$mlik[[1]]
r10 = inla(formula10, data = data, family = "gaussian")$mlik[[1]]
r11 = inla(formula11, data = data, family = "gaussian")$mlik[[1]]

mlik = c(r00, r01, r10, r11)
mlik = mlik - max(mlik)
p = exp(mlik) / sum(exp(mlik))

prob.z = p[2] + p[4]
prob.zz = p[3] + p[4]

print(c(prob.z, prob.zz))

###
