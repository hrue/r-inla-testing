rt.scaled = function(n, dof)
{
    sigma = sqrt(dof/(dof-2))
    x = rt(n, df=dof)
    return (x/sigma)
}

n=100
dof = 10
y = rt.scaled(n, dof)

r = inla(y ~ 1,
        data = data.frame(y),
        family = "t",
        control.family = list(
                hyper = list(
                        prec = list(
                                initial = 0,
                                fixed = TRUE),
                        dof = list(
                                prior = "dof",
                                param = c(10, 0.25)))), 
        verbose=TRUE)

        
