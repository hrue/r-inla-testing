library(INLA)
library(sparseinv)
inla.setOption(num.threads = "1:1")

Q0 <- as(matrix(1, 1, 1), "CsparseMatrix")
tref0 <- Sys.time()
S_partial = inla.qinv(Q0)
tref0 <- Sys.time() - tref0
cat("inv0 ", tref0,  "\n")

for (gnm in c("inla_graph_small", "inla_graph_medium", "inla_graph_larger")) {
    
    ## plot with
    if (FALSE) inla.spy(gnm)

    cat("\nread ",  gnm,  "\n")
    Q=inla.graph2matrix(inla.read.graph(gnm))
    diag(Q)=2*nrow(Q)

    inla.setOption(smtp = "taucs")
    tref <- Sys.time()
    S_partial = inla.qinv(Q)
    tref <- Sys.time() - tref - tref0
    cat("taucs part.inv \n")
    print(tref)
    cat("\n")

    inla.setOption(smtp = "pardiso")
    tref <- Sys.time()
    S_partial = inla.qinv(Q)
    tref <- Sys.time() - tref - tref0
    cat("pardiso part.inv\n")
    print(tref)
    cat("\n")

    Q <- as(Q, "CsparseMatrix")
    tref <- Sys.time()
    X <- cholPermute(Q)
    S_partial = Takahashi_Davis(Q, cholQp = X$Qpermchol, P = X$P)
    tref <- Sys.time() - tref
    cat("cholmod part.inv\n")
    print(tref)
    cat("\n\n")
}
