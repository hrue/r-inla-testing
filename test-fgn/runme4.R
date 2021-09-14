library(INLA)
library(longmemo)
n = 1000
H1 = 0.6
H2 = 0.8
y1 = simFGN0(n, H1)
y1 = y1 - mean(y1)

y2 = simFGN0(n, H2)
y2 = y2 - mean(y2)
y=y1+y2

r = inla(y ~ -1 + f(idx, model="fgn", hyper = list(H = list(initial = -1)))+
             f(idx2, model="fgn", hyper = list(H = list(initial = 1))), 
         data = data.frame(y, idx=1:n,idx2=1:n),
         control.family =list(hyper = list(prec = list(initial = 12, fixed=TRUE))),verbose=T)
