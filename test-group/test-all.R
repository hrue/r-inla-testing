library(INLA)
inla.setOption(smtp = "taucs")

nr <- 3
ng <- 10
n <- 10 * nr * ng
graph <- inla.read.graph(matrix(1, ng, ng))
y <- rnorm(n)

idx <- sample(1:n, n, replace = TRUE)
rr <- sample(1:nr, n, replace = TRUE)
gg <- sample(1:ng, n, replace = TRUE)

for (m in c("rw1", "rw2", "exchangeable", "exchangeablepos", "iid")) {
    r1 <- inla(y ~ -1 + f(idx, model = "iid", hyper = list(prec = list(initial = 0, fixed = TRUE)), 
                          replicate = rr, nrep = nr, group = gg, ngroup = ng, control.group = list(model = m)), 
               data = data.frame(y, idx, rr, gg), family = "stdnormal")
    r2 <- inla(y ~ -1 + f(idx, model = "iid", hyper = list(prec = list(initial = 0, fixed = TRUE)),
                          replicate = rr, nrep = nr, group = gg, ngroup = ng, control.group = list(model = m)), 
               data = data.frame(y, idx, rr, gg), family = "stdnormal", inla.call = "inla.mkl.work")

    cat(m, r1$mlik[1]-r2$mlik[1], "\n")
}

for (m in c("ar")) {
    r1 <- inla(y ~ -1 + f(idx, model = "iid", hyper = list(prec = list(initial = 0, fixed = TRUE)), 
                          replicate = rr, nrep = nr, group = gg, ngroup = ng, control.group = list(model = m, order = 2)), 
               data = data.frame(y, idx, rr, gg), family = "stdnormal")
    r2 <- inla(y ~ -1 + f(idx, model = "iid", hyper = list(prec = list(initial = 0, fixed = TRUE)),
                          replicate = rr, nrep = nr, group = gg, ngroup = ng, control.group = list(model = m, order = 2)), 
               data = data.frame(y, idx, rr, gg), family = "stdnormal", inla.call = "inla.mkl.work")

    cat(m, r1$mlik[1]-r2$mlik[1], "\n")
}

for (m in c("besag")) {
    r1 <- inla(y ~ -1 + f(idx, model = "iid", hyper = list(prec = list(initial = 0, fixed = TRUE)), 
                          replicate = rr, nrep = nr, group = gg, ngroup = ng, control.group = list(model = m, graph = graph)), 
               data = data.frame(y, idx, rr, gg), family = "stdnormal")
    r2 <- inla(y ~ -1 + f(idx, model = "iid", hyper = list(prec = list(initial = 0, fixed = TRUE)),
                          replicate = rr, nrep = nr, group = gg, ngroup = ng, control.group = list(model = m, graph = graph)), 
               data = data.frame(y, idx, rr, gg), family = "stdnormal", inla.call = "inla.mkl.work")

    cat(m, r1$mlik[1]-r2$mlik[1], "\n")
}
