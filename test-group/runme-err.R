
r = inla(y ~ f(idx, group = g,  ngroup= ng), 
    data = list(y = 1:10,  idx = 1:10, g = 1+1:10, ng = 30))

