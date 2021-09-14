n = 10
y = rnorm(n)
s2 = sum((y - mean(y))^2)/(n-1)

dic = -n/2 * log(2*pi)

r = inla(y ~ 1, data = data.frame(y),
    control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE))), 
    control.compute=list(dic=TRUE, po=TRUE, cpo=TRUE, waic=TRUE))

## dic
p.eff = 1
dic = -2*sum(dnorm(y, mean = mean(y), sd = 1, log=TRUE)) + 2*p.eff

print(cbind(dic.true = dic,  dic = r$dic$dic, abs.err = abs(dic -r$dic$dic)))
print(cbind(p.eff.true = p.eff,  p.eff = r$dic$p.eff, abs.err = abs(p.eff - r$dic$p.eff)))


## waic

log.post = -n/2*log(2*pi) - n/2 * log(1+1/n) - 1/2 * n*(n-1)/(n+1) * s2
p.waic = (n-1)/n * s2 + 1/(2*n)
waic = -2*log.post + 2*p.waic

print(cbind(waic.true = waic,  waic = r$waic$waic, abs.err = abs(waic - r$waic$waic)))
print(cbind(p.eff.true = p.waic,  p.eff = r$waic$p.eff, abs.err = abs(p.waic - r$waic$p.eff)))

