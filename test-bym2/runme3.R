phi = 1/(1+exp(-seq(-10, 10, len=1000)))
for(graph in system.file("demodata/germany.graph", package="INLA")) {
    prior = INLA:::inla.pc.bym.phi(graph = graph)
    dev.new()
    plot(phi, prior(phi), type="l",  lwd=2, main = graph)
}


    
