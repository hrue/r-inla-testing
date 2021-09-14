### "cut" version of Håvard's node-split function



inla.cd = function(formula, data, split.by, correct=FALSE, debug=FALSE, ...)
{
    my.debug = function(...) if (debug) cat("*** debug *** inla.cd: ", ... , "\n")
    ##cat("CUT:")
    require(INLA)
    suppressMessages(require(MASS))
    stopifnot(!missing(formula))
    stopifnot(!missing(data))
    stopifnot(!missing(split.by))
    ## use an INLA internal to get the variable names of the whole model, ie, the fixed effects
    ## + f(idx)'s
    intf = INLA:::inla.interpret.formula(formula)
    ## check that fixed/random effects exists, otherwise you can get an error e.g.
    ## for a model with only fixed effects:
    if (!is.null(intf$fixf)) {
        fixf <- as.character(attr(terms.formula(intf$fixf), "variables"))[-c(1:2)]
    } else fixf <- NULL
    if (!is.null(intf$randf)) {
        randf <- as.character(attr(terms.formula(intf$randf), "variables"))[-c(1:2)]
    } else randf <- NULL    
    vars = c(fixf, randf)
    
    
    ## find the name of the response, store the response and remove it from the data.frame.
    ## merge the two dataframes and add the reponse back in
    resp = formula[[2]]
    stopifnot(is.name(resp))
    resp = as.character(resp)
    
    ## extract the variable which define how to split
    n = dim(data)[1]
    split.val = data[, split.by == names(data)]
    split.uval = sort(unique(split.val))
    split.len = length(split.uval)
    stopifnot(split.len > 0)
    
    ## store the results
    result = rep(list(list()), split.len)
    ## store inla-results
    res <- list()
    r = NULL
    p.linpred <- numeric(split.len)
    for(split.idx in seq_len(split.len)) {
   ## for (split.idx in 11) {
    my.debug(split.idx)
        ##print(Sys.time())
        ## determine group idx's
        idx = (split.val == split.uval[split.idx])
        
        ## First run REP version, everything except part i, "between" group:
        
        data.rep <- data
        data.rep[idx, resp] <- NA
        
        ## prepare the arguments for inla()
        args = list(...)
        args$data = data.rep
        args$formula = formula
        cont.compute = args$control.compute
        if (is.null(cont.compute)) cont.compute = list()
        cont.compute$config = TRUE
        cont.compute$return.marginals = TRUE
        args$control.compute = cont.compute
        ## make linear combinations for calculating linear predictor-based p-values:
        cont.inla <- args$control.inla
        cont.inla$lincomb.derived.correlation.matrix = TRUE
        args$control.inla <- cont.inla
        n.pred <- sum(idx)
        lc.rep <- c()
        for (i in 1:n.pred) {
            pred.idx <- rep(NA,n)
            pred.idx[which(idx)[i]] <- 1
            lci <- inla.make.lincomb(Predictor = pred.idx)
            names(lci) <- paste0("lc.pred",i)
            lc.rep <- c(lc.rep, lci)
        }
        args$lincomb = lc.rep
        r.rep = do.call("inla", args = args)
    if (correct) {
       ## browser()
            idx.num <- which(idx)
            mu.rep <-  r.rep$summary.linear.predictor$mean[idx.num]
            for (j in 1:length(mu.rep)) mu.rep[j] <- qnorm(inla.pmarginal(mu.rep[j],
                                                                          r.rep$marginals.linear.predictor[[idx.num[j]]]))
            sigma.rep <- r.rep$misc$lincomb.derived.correlation.matrix
        } else {
            mu.rep <- r.rep$summary.lincomb.derived$mean
            sigma.rep <- r.rep$misc$lincomb.derived.covariance.matrix
        }
       
        ## Then run LIK version, group i, within-group:
        
        data.lik <- data[idx,]
        if (!is.null(args$E)) args$E <- args$E[idx]
        if (!is.null(args$Ntrials)) args$Ntrials <- args$Ntrials[idx]
        args$data <- data.lik
         lc.lik <- c()
        ## make new linear combinations:
         for (i in 1:n.pred) {
          pred.idx <- rep(NA,nrow(data.lik))
          pred.idx[i] <- 1
          lci <- inla.make.lincomb(Predictor = pred.idx)
          names(lci) <- paste0("lc.pred",i)
          lc.lik <- c(lc.lik, lci)
      }

        args$lincomb = lc.lik
        ## set prior of hyperpar to posterior from REP version:
        args$control.update = list(result = r.rep)
##        args$verbose=TRUE
        ## run INLA:
        ##browser()
        r.lik <- do.call("inla", args = args)
        ## cat(round(as.numeric(r.lik$cpu.used["Total"]), 3), "sec", "... ")
        if (correct) {
            mu.lik <- r.lik$summary.linear.predictor$mean
            ##browser()
            for (j in 1:length(mu.lik)) mu.lik[j] <- qnorm(inla.pmarginal(mu.lik[j],
                                                                          r.lik$marginals.linear.predictor[[j]]))
            sigma.lik <- r.lik$misc$lincomb.derived.correlation.matrix
        } else {
            mu.lik <- r.lik$summary.lincomb.derived$mean
            sigma.lik <- r.lik$misc$lincomb.derived.covariance.matrix
        }
        mu.diff <- mu.rep - mu.lik
        sigma.diff <- sigma.rep + sigma.lik
        respIdx <- which(names(data.lik)==resp)
        curr.data <- data.lik[,-respIdx]
    curr.data[is.na(curr.data)] <- 0
    ## curr.data: change any factors to numeric:
        for (kk in 1:ncol(curr.data))
            if (is.factor(curr.data[,kk]))
                curr.data[,kk] <- as.numeric(as.character(curr.data[,kk]))
       
       ##lin.pred.df <- dof[split.idx]
        ##lin.pred.df <- 5
        ##print(curr.data)
        ##lin.pred.df <- dof
        ## Calculate p-values from linear predictor:
        lin.pred.mu <- mu.diff
        lin.pred.sigma <- sigma.diff
         lin.pred.df <- sum(eigen(lin.pred.sigma)$values > 1e-3)
        lin.pred.Delta <- t(lin.pred.mu) %*% ginv(lin.pred.sigma, tol=1e-3) %*% lin.pred.mu
    p.linpred[split.idx] <- 1 - pchisq(as.numeric(lin.pred.Delta), df=lin.pred.df) 
        my.debug(split.idx,"of",split.len,": p-value:",round(p.linpred[split.idx],4),"df:",lin.pred.df)
        ## res[[split.idx]] <- list()
    ##     ##res[[split.idx]]$r.rep <- r.rep
    ##     ##res[[split.idx]]$r.lik <- r.lik
    ##     res[[split.idx]]$p <- p.linpred[split.idx]
    ##     res[[split.idx]]$mu.rep <- mu.rep
    ##     res[[split.idx]]$mu.lik <- mu.lik
    ##     res[[split.idx]]$mu.diff <- mu.diff        
    ##     res[[split.idx]]$sigma.rep <- sigma.rep
    ##     res[[split.idx]]$sigma.lik <- sigma.lik
    ##     res[[split.idx]]$sigma.diff <- sigma.diff
    ##     res[[split.idx]]$Delta <- lin.pred.Delta
    ## res[[split.idx]]$dof <- lin.pred.df
    ##browser()
        ##print(eigen(lin.pred.sigma)$values)
            }
    return(p.linpred)
}
