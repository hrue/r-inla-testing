r = inla(y ~ 1 + f(idx),
        data = data.frame(y=1:10, idx = 1:10))

rr = inla(y ~ 1 + f(idx),
        data = data.frame(y=1:10, idx = 1:10),
        control.update = list(result = r),
        verbose=TRUE)



rr
