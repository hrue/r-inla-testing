## this is a prototype implementation of the node-split model validation idea. the model is
## splitted in the 'within group' and 'between group' using the split.by variable name, but
## hyperparameters in the likelihood are shared.

## check the new.formula, not all models do work...


node.split = function(formula, data, split.by, nsample = 300, ...)
{
    stopifnot(!missing(formula))
    stopifnot(!missing(data))
    stopifnot(!missing(split.by))
    stopifnot(any(split.by == names(data)))
    stopifnot(attr(terms.formula(formula), "intercept") == 0)
    
    ## use an INLA internal to get the variable names of the whole model, ie, the fixed effects
    ## + f(idx)'s
    intf = INLA:::inla.interpret.formula(formula)
    vars = (c(as.character(attr(terms.formula(intf$fixf), "variables"))[-c(1:2)],
              as.character(attr(terms.formula(intf$randf), "variables"))[-c(1:2)]))

    ## function to rename variables,  so its 'idx' and 'idx.SPLIT'
    map.name = function(v) {
        return (paste(v, ".SPLIT", sep=""))
    }
    
    ## create a version of the formula with .SPLIT names
    new.vars = map.name(vars)
    new.formula = as.character(formula[[3]])
    if (length(new.formula) == 1) {
        ##
    } else if (length(new.formula) == 3) {
        new.formula = paste(new.formula[2], new.formula[1], new.formula[3])
    } else {
        print(new.formula)
        stop("This should not happen")
    }
    for(idx in seq_along(vars)) {
        new.formula = gsub(vars[idx], new.vars[idx], new.formula)
    }
    ## and the merge it to the old one
    new.formula = update(formula, as.formula(paste(". ~ . + ", new.formula)))
    print(new.formula)
    
    ## find the name of the response, store the response and remove it from the data.frame.
    ## merge the two dataframes and add the reponse back in
    resp = formula[[2]]
    stopifnot(is.name(resp))
    resp = as.character(resp)
    Y.col = which(resp == names(data))
    Y = data[, Y.col]
    new.data = data = data[, -Y.col]
    ## only change the vars-names,  not others
    nms = names(data)
    for (i in seq_along(nms)) {
        if (any(nms[i] == vars))
            nms[i] = map.name(nms[i])
    }
    names(new.data) = nms
    new.data = inla.rbind.data.frames(data, new.data)
    new.data[, resp] = c(Y, Y)
    
    ## extract the variable which define how to split
    n = dim(data)[1]
    split.val = data[, split.by == names(data)]
    split.uval = unique(split.val)
    split.len = length(split.uval)
    stopifnot(split.len > 0)
    
    ## store the results
    result = rep(list(list()), split.len)
    ## store inla-results
    r = NULL
    for(split.idx in seq_len(split.len)) {
        cat("\t", "do ", split.idx, " of ", split.len, "... ")

        ## determine group idx's
        idx = (split.val == split.uval[split.idx])

        ## this define group and not-group. so first part of the model, is the group only, and
        ## the second part is the rest (not-group)
        new.idx = c(idx, !idx)

        ## then we set the reponse to NA, for the not-group in the first part, and the group, in
        ## the second part.
        tmp.data = new.data
        tmp.data[!new.idx, which(names(tmp.data) == resp)] = NA

        ## prepare the arguments for inla()
        args = list(...)
        args$data = tmp.data
        args$formula = new.formula
        ## need to force config=TRUE in argument control.group
        cont.compute = args$control.compute
        if (is.null(cont.compute)) cont.compute = list()
        cont.compute$config = TRUE
        args$control.compute = cont.compute
        ## use the previous computed fit as the initial value
        if (exists("r")) 
            args$control.mode = list(x = r$mode$x, theta = r$mode$theta, restart=TRUE)

        ## finally..
        r = do.call("inla", args = args)
        cat(round(as.numeric(r$cpu.used["Total"]), 3), "sec", "\n")
        rx = inla.posterior.sample(nsample, r)

        len.group = length(which(idx))
        w.group = matrix(NA, len.group, nsample)
        b.group = matrix(NA, len.group, nsample)

        for(i in 1:nsample) {
            lin.pred = rx[[i]]$latent[1:(2*n)]
            w.group[, i] = lin.pred[which(idx)]
            b.group[, i] = lin.pred[n + which(idx)]
        }
        result[[split.idx]] = list(
                  split.by = split.by,
                  split.value = split.uval[split.idx], 
                  within.group.linpred = w.group,
                  between.group.linpred = b.group)
    }

    return(result)
}
