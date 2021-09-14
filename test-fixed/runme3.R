r = inla(y ~ 1,
        data = data.frame(y=1),
        control.inla = list(int.strategy = "eb"),
        control.compute = list(config=TRUE),
        inla.call = "inla.work")
r$mode$theta
rr = inla(y ~ 1,
        data = data.frame(y=1),
        control.inla = list(int.strategy = "eb"),
        control.mode = list(result = r,  fixed=TRUE), 
        control.compute = list(config=TRUE), 
        inla.call = "inla.work")

print( r$misc$configs$config[[1]]$Q )
print(rr$misc$configs$config[[1]]$Q )

print(log(r$misc$config$config[[1]]$Q[1, 1] - exp(11)))
print(log(rr$misc$config$config[[1]]$Q[1, 1] - exp(11)))
