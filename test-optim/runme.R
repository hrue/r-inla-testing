while(TRUE) {
    n = 10
    x = runif(n)
    y = 1 + x + rt(n, df=2)
    
    ##inla.setOption(inla.call=inla.call.builtin())
    r = inla(y ~ 1 + x,  family = "t",
            data = data.frame(y, x),
            control.fixed = list(prec.intercept = .01, prec = .01), silent=2L)

    if (is.null(r$ok)) {
        print("FAIL")
        r = inla(y ~ 1 + x,  family = "t",
                data = data.frame(y, x),
                control.fixed = list(prec.intercept = .01, prec = .01), 
                verbose=TRUE, keep=TRUE,
                control.inla = list(verbose=TRUE))
        stop("XXX")
    } else {
        print(runif(1))
    }
    
}

