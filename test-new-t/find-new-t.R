
new.dt = function(x, df, prec = 1, prec.tail = .01)
{
    limit = sqrt(df)
    result = c()
    
    for(xx in x) {
        xx = abs(xx) * sqrt(prec)

        if (xx < limit) {
            result = c(result,  dt(xx, df=df, log=TRUE) + 0.5*log(prec))
        } else {           
            dif = -0.5*(df+1)/sqrt(df)
            result = c(result,
                    dt(limit, df=df, log=TRUE) + 0.5*log(prec) - 
                    0.5*prec.tail*(xx-limit)^2 + dif*(xx-limit))
        }
    }
    return (result)
}
    
        
par(mfrow=c(4, 2))
prec = 10
x = seq(0, 7, len=1000)

for(df in 2:5) {

    plot(x, (new.dt(x/sqrt(prec),df=df, prec=prec)), type="l")
    lines(x, (dt(x, df=df, log=TRUE) + 0.5*log(prec)), lty=2, lwd=2)
    title(paste("df = ", df))

    plot(x, exp(new.dt(x/sqrt(prec),df=df, prec=prec)), type="l")
    lines(x, exp(dt(x, df=df, log=TRUE) + 0.5*log(prec)), lty=2, lwd=2)
    title(paste("df = ", df))

}
