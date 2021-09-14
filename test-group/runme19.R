n = 10
y = (1:n)/n + rnorm(n)
idx = 1:n
g = 1:n

r = inla(y~ -1 + f(idx, model="iid",
                   group = g,
                   control.group = list(model="iid"),
                   hyper = list(
                       prec = list(
                           initial = 10,
                           fixed=FALSE))), 
         control.family = list(hyper = list(prec = list(initial = 1,
                                                        fixed=FALSE))), 
         data = data.frame(y, idx, g), 
         control.compute = list(dic=TRUE, waic=TRUE),
         verbose=F,
         num.threads = 4)
rr = inla(y~ -1 + f(idx, model="iid",
                    replicate = g, 
                    hyper = list(
                        prec = list(
                            initial = 10,
                            fixed=FALSE))), 
          control.family = list(hyper = list(prec = list(initial = 1,
                                                         fixed=FALSE))), 
          data = data.frame(y, idx, g), 
          control.compute = list(dic=TRUE, waic=TRUE),
          verbose=F,
          num.threads = 4)

iidx = 1:n^2
rrr = inla(y~ -1 + f(idx, model="iid",
                     values = iidx, 
                     hyper = list(
                         prec = list(
                             initial = 10,
                             fixed=FALSE))), 
           control.family = list(hyper = list(prec = list(initial = 1,
                                                          fixed=FALSE))), 
           data = list(y=y, idx=idx, iidx=iidx, g=g), 
           control.compute = list(dic=TRUE, waic=TRUE),
           verbose=F,
           num.threads = 4)

print(r$waic$waic - rr$waic$waic)
print(r$waic$waic - rrr$waic$waic)
print(cbind(r=r$waic$local.waic, rr=rr$waic$local.waic, rrr=rrr$waic$local.waic))
print(r$dic$dic - rr$dic$dic)
print(r$dic$dic - rrr$dic$dic)
print(r$mlik - rr$mlik)
print(r$mlik - rrr$mlik)

head(r$logfile[grep("val_prev", r$logfile)])
head(rr$logfile[grep("val_prev", rr$logfile)])
head(rrr$logfile[grep("val_prev", rr$logfile)])

