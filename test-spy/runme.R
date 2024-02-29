g = inla.graph2matrix(system.file("demodata/germany.graph", package="INLA"))

inla.spy(g)
title("g")
dev.new()
inla.spy(cbind(g, g))
title("cbind(g, g)")
dev.new()
inla.spy(rbind(g, g))
title("rbind(g, g)")


for (rr in inla.reorderings()) {
    dev.new()
    r <- inla.qreordering(g, rr)
    inla.spy(g, reordering = r)
    title(rr)
}


