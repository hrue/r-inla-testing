data(Tokyo)
Tokyo=Tokyo[-which(Tokyo$n==1),]
formula = y ~ f(time, model="iid",
                hyper = list(prec = list(initial = 0, fixed = T)))- 1
result = inla(formula,
              family="binomial",
              Ntrials=n,
              verbose = T, 
              data=cbind(Tokyo, w = w))
plot(result)

