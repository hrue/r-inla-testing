inla.setOption(inla.mode = "experimental")
inla.setOption(inla.call = "inla.mkl.work")

n <- 10
ng <- 3
nr <- 2

r <- inla(y ~ -1 + f(idx,
                     model = "rw2",
                     values = 1:n,
                     group = 1,
                     ngroup = ng,
                     replicate = 1,
                     nrep = nr,
                     
                     vb.correct = c(1, 4, 7)),
          data = data.frame(y = 0,
                            idx = 1),
          family = "t", 
          control.inla = list(control.vb = list(enable = TRUE, f.enable.limit = c(3, 3))), 
          keep = TRUE, 
          safe = FALSE, 
          verbose = TRUE)

          
                     
