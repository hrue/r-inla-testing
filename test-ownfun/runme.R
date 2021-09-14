ownfun = function(file.ini,
                  file.log,
                  results.dir,
                  inla.call.args) 
{
    ## 'results.dir' is not used as the default inla.call will automatically
    ## do the right thing.
    inla.call = INLA:::inla.call.builtin()
    if (is.null(file.log)) {
        ret = system(paste(shQuote(inla.call), inla.call.args,
                           shQuote(file.ini)))
    } else {
        ret = system(paste(shQuote(inla.call), inla.call.args,
                           shQuote(file.ini), " > ", file.log))
    }
    return (ret)
}


y = 1:10
r = inla(y ~ 1,
         verbose = FALSE,
         data = data.frame(y), 
         inla.call = ownfun)
