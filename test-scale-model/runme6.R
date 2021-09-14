g = inla.read.graph("scotland.graph")
n = g$n
y = rep(0, n)
idx = 1:n
r = inla(y~-1 + f(idx,  model="besag", graph = g, scale.model=TRUE,  adjust.for.con.comp=TRUE),
         data = data.frame(y, idx),
         verbose=TRUE, debug=TRUE)
