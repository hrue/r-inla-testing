library(INLA)
library(sp)



    data(PRprec, package='INLA')
    dim(PRprec)
    n <- nrow(PRprec)
    PRprec[1:3, 1:7]

    crs.ll <- inla.CRS('+proj=longlat +ellps=WGS84')
    crs.mollkm <- inla.CRS('+proj=moll +units=km')
    prec.avg <- SpatialPointsDataFrame(
        spTransform(
            SpatialPoints(
                as.matrix(PRprec[,1:2]),
                crs.ll), crs.mollkm),
        data=data.frame(
            elevation=PRprec[,3],
            prec=rowMeans(PRprec[, 4:368])))

    head(prec.avg)
    summary(prec.avg)

    boundary <- inla.nonconvex.hull(
        coordinates(prec.avg), 20, 20)

    mesh1 <- inla.mesh.2d(
        boundary=boundary,
        max.edge=c(15, 25),
        cutoff=5,
        offset=100,
        max.n.strict = -1, 
        n=20)
    mesh1$n
    mesh2 <- inla.mesh.2d(
        boundary=boundary,
        max.edge=c(15, 25)/2,
        cutoff=.5,
        offset=100,
        max.n.strict = -1, 
        n=20)
    mesh2$n
    mesh3 <- inla.mesh.2d(
        boundary=boundary,
        max.edge=c(15, 25)/4,
        cutoff=0.05,
        offset=100,
        max.n.strict = -1, 
        n=20)
    mesh3$n

    ## projector
    A1 <- inla.spde.make.A(mesh1, coordinates(prec.avg))
    A2 <- inla.spde.make.A(mesh2, coordinates(prec.avg))
    A3 <- inla.spde.make.A(mesh3, coordinates(prec.avg))

    d.stack <- inla.stack(
        tag='est',
        data=list(y=prec.avg$prec), 
        effects=list(
            b0=rep(1, n), 
            s1=1:ncol(A1), 
            s2=1:ncol(A2), 
            s3=1:ncol(A3)), 
        A=list(1, A1, A2, A3)
    )
#################################################
#### consider covariates in the covariance
#################################################

### define the 'covariates' at the mesh nodes
#    llloc <- coordinates(spTransform(
 #       SpatialPoints(mesh$loc[,1:2], crs.mollkm), crs.ll))
  #  B0 <- cbind(west=llloc[,1]<(-51.5),
   #             north=llloc[,2]>(-24.5))

    lk0 <- log(8)/2
    lt0 <- -log(4*pi)/2


    nsspde1 <- inla.spde2.matern(
        mesh=mesh1, alpha=2,
        B.kappa=cbind(lk0, 0, 1),
        B.tau  =cbind(lt0, 1, 0))

    nsspde2 <- inla.spde2.matern(
        mesh=mesh2, alpha=2,
        B.kappa=cbind(lk0, 0, 1),
        B.tau  =cbind(lt0, 1, 0))

    nsspde3 <- inla.spde2.matern(
        mesh=mesh3, alpha=2,
        B.kappa=cbind(lk0, 0, 1),
        B.tau  =cbind(lt0, 1, 0))

    fns1 <- y ~ 0 + b0 +
        f(s1, model=nsspde1) +
        f(s2, model=nsspde2) +
        f(s3, model=nsspde3) 

    fns2 <- y ~ 0 + b0 +
        f(s3, model=nsspde3) +
        f(s2, model=nsspde2) +
        f(s1, model=nsspde1) 
        

    pprec <- list(prior='pc.prec', param=c(1, 0.5),
                  initial = 4, fixed = TRUE)

fitns1 <- inla(
    formula=fns1,
    data=inla.stack.data(d.stack), 
    control.predictor=list(
        A=inla.stack.A(d.stack)),
    control.family=list(hyper=list(theta=pprec)),
    verbose = TRUE)

fitns2 <- inla(
    formula=fns2,
    data=inla.stack.data(d.stack), 
    control.predictor=list(
        A=inla.stack.A(d.stack)),
    control.family=list(hyper=list(theta=pprec)),
    verbose = TRUE)
