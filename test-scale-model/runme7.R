g = inla.read.graph("test1.graph")
n = g$n
y = rep(0, n)
idx = 1:n
opts = c(TRUE, TRUE)
r = inla(y~-1 + f(idx,  model="besag", graph = g, scale.model=opts[1],  adjust.for.con.comp=opts[2],
                  diagonal = 1, debug=TRUE, 
                  hyper = list(prec = list(initial = 1, fixed=TRUE))), 
         data = data.frame(y, idx),
         family = "gaussian",
         control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))),
         control.compute = list(config=TRUE), 
         verbose=F, debug=F)
Q = inla.extract.Q("idx", y~-1 + f(idx,  model="besag", graph = g, scale.model=opts[1],  adjust.for.con.comp=opts[2],
                  diagonal = 1e-5, 
                  hyper = list(prec = list(initial = 1, fixed=TRUE))), 
         data = data.frame(y, idx))
print(Q)
