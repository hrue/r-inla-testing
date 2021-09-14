
## just check the internal definitions of the to.theta and from.theta
## functions
inla.my.update()

models = inla.models()
for(sec in 1:length(models)) {
    s = models[[sec]]
    for(m in 1:length(s)) {
        model = s[[m]]
        hyper = model$hyper

        len.hyper = length(hyper)
        if (len.hyper > 0) {
            for(h in 1:len.hyper) {

                h = hyper[[h]]
                initial = h$initial
                if (!is.null(h$to.theta)) {
                    print(paste(names(s)[m], initial,
                                h$to.theta(h$from.theta(initial))))
                }
            }
        }
    }
}


        
