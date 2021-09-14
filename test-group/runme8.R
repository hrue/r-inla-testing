gfnm = "group.graph"
g = inla.read.graph(gfnm)

n = g$n
y = rnorm(n) + 1:n
idx = rep(1, n)
group = 1:n

formula = y ~ -1 + f(idx, model="iid", group = group,
        control.group = list(graph = g, model="besag", scale.model=TRUE))
r = inla(formula,  data = data.frame(y, idx, group),
        control.family = list(initial = 0, fixed = TRUE), 
        verbose = TRUE)

fformula = y ~ -1 + f(group, model="besag", constr=FALSE, diagonal = 0, graph=g, scale.model=TRUE)
rr = inla(fformula,  data = data.frame(y, idx, group), 
        control.family = list(initial = 0, fixed = TRUE))

        
