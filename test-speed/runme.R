
run = function(nrep=1) 
{
    data(Germany)
    g = system.file("demodata/germany.graph", package="INLA")
    G = inla.graph2matrix(g)
    ##
    if (FALSE) {
        for(k in seq_len(nrep)) {
            G = bdiag(G, G)
            Germany = rbind(Germany,Germany)
        }
    } else {
        Q = inla.as.sparse(matrix(1, nrep, nrep))
        G = inla.as.sparse(kronecker(G,  Q))
        Germ = Germany
        for(k in seq_len(nrep-1)) {
            Germany = rbind(Germany,Germ)
        }
    }
    
    Germany$region = 1:nrow(Germany)
    Germany$region.struct = Germany$region
    formula3 = Y ~ f(region.struct,model="besag",graph=G, scale.model=TRUE) +
        f(region,model="iid") + f(x, model="rw2", scale.model=TRUE)
    r = inla(formula3,
             family="poisson",
             data=Germany,
             E=E,
             control.inla = list(int.strategy = "eb", tolerance = 1e-12), 
             num.threads = 1, 
             verbose=FALSE)
    xx = as.numeric(strsplit(r$logfile[grep("^Number of", r$logfile)], " ")[[1]])
    nf = as.numeric(xx[which(!is.na(xx))])
    cpu = as.numeric(r$cpu.used[2])
    mode = as.numeric(r$mode$theta[1] + 10 * r$mode$theta[2] + 100 * r$mode$theta[3])
    res = c(cpu, mode, nf)
    return(res)
}
 
result = NULL
for (nrep in c(1, 5, 10, 15, 20)) {
    for(pardiso in c(TRUE, FALSE)) {
        inla.setOption(pardiso.license=if (pardiso) "~/sys/licenses/pardiso.lic" else NULL)
        for(mkl in c(TRUE, FALSE)) {
            inla.setOption(mkl=mkl)
            for(static in c(TRUE, FALSE)) {
                if (static) {
                    INLA:::inla.dynload.workaround()
                } else {
                    inla.setOption(inla.call = INLA:::inla.call.builtin())
                }
                r = run(nrep)
                res = c(size = nrep,
                        solver=if (pardiso) "pardiso" else "taucs",
                        linalg = if (mkl) "MKL" else "OpenBLAS",
                        type = if (static) "static" else "dynamic",
                        tmode = round(r[2], digits = 4), 
                        nfunc = as.integer(r[3]), 
                        cpu = round(r[1], digits=3))
                result = rbind(result, result = res)
                o = order(as.numeric(result[, "cpu"]))
                result = result[o, ]
                cat("\n\n")
                print(result)
            }
        }
    }
}

