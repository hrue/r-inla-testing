inla.my.update()
inla.setOption("inla.call", "inla.work")

n = 10

y = c(1:n, 2, 1, 0)
i = c(1:n, NA,NA,NA)
ii = c(1:n, NA,NA,NA)
j = c(rep(NA,n), 1, 0, 0)
jj = c(rep(NA,n), 0, 1, 0)
jjj = c(rep(NA,n), 0, 0, 1)

formula = y ~ f(i) + f(ii, constr=T, values=1:(2*n), param=c(1,1)) -1 +
    f(j, copy="i", fixed=FALSE) +
    f(jj, copy="i", same.as="j") +
    f(jjj, copy="ii", same.as="j") 

r = inla(
        formula,
        data = data.frame(y,i,j,jj,jjj),
        control.data = list(initial=12,fixed=T)
        )
