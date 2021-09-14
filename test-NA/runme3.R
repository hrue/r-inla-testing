dat=list(year=factor(c(1,2,NA)),y=1:3)
r = inla(formula=y~0+f(year,model="iid"),data=dat,
        control.fixed=list(expand.factor.strategy="inla"))
