data(Germany)
g = inla.read.graph(system.file("demodata/germany.graph", package="INLA"))
source(system.file("demodata/Bym-map.R", package="INLA"))
Germany = cbind(Germany,region.struct=Germany$region)

g <- inla.graph2matrix(g)
g <- inla.matrix2graph(bdiag(g, g))
n <- g$n
nc <- 300
A <- matrix(rnorm(n*nc), nc, n)
A[1, ] <- 1
e <- rnorm(nc)
e[1] <- 0

formula3 = Y ~ f(region.struct,model="besag",graph=g, constr = FALSE, 
                 extraconstr = list(A = A, e = e),
                 scale.model = TRUE) +
               f(region,model="iid") + f(x, model="rw2", scale.model = TRUE)

Sys.setenv(INLA_TRACE = "GMRFLib_solve_llt_sparse_matrix:10")
inla.setOption(smtp = 'taucs', num.threads = "10:1", safe = FALSE)
if (!FALSE) {
    r = inla(formula3,family="poisson",data=Germany,E=E, verbose = !FALSE)
    print(r$cpu.used)
    print(r$cpu.intern)
    ##r <- inla.rerun(r)
}

INLA:::inla.my.update(b = TRUE)
rr = inla(formula3,family="poisson",data=Germany,E=E, verbose = !FALSE, keep = FALSE)
print(rr$cpu.used)
print(rr$cpu.intern)
stop("XXXXXXXXXXXXXXXXXXXXXXXXXXX")
rr <- inla.rerun(rr)

inla.setOption(smtp = 'stiles')
rrr = inla(formula3,family="poisson",data=Germany,E=E)
print(rrr$cpu.used)
print(rrr$cpu.intern)
rrr = inla.rerun(rrr)


r$mlik - rr$mlik
r$mlik - rrr$mlik
mean(abs(r$summary.random$region.struct$mean - rr$summary.random$region.struct$mean))
mean(abs(r$summary.random$region.struct$mean - rrr$summary.random$region.struct$mean))
mean(abs(r$summary.random$region.struct$sd/rr$summary.random$region.struct$sd - 1))
mean(abs(r$summary.random$region.struct$sd/rrr$summary.random$region.struct$sd - 1))

