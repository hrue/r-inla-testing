runme3 = function(file = "testing.dat", msg)
{
    xx = try(scan(file), silent=TRUE)
    if (inherits(xx, "try-error")) {
        ff = file(file, "rb")
        x = readBin(ff, integer(), 3L)
        y = readBin(ff, double(), x[1] + x[2] + x[1]*x[2])
        xx = c(x, y)
        close(ff)
    }
    nx = xx[1]
    ny = xx[2]
    nz = xx[3]
    x = xx[3 + 1:nx]
    y = xx[3 + nx + 1:ny]
    z = xx[3 + nx + ny + 1:nz]
    z[is.infinite(z)] = NA
    Z = matrix(z, nx, ny)
    inla.display.matrix(Z)
    if (!missing(msg))
        title(msg)
    
    return (invisible(list(x=x, y=y, z=Z)))
}

runme4 = function() {
    dev.new()
    log.prior = runme3("testing1.dat", "log.prior")
    dev.new()
    level = runme3("testing2.dat", "level")
    dev.new()
    len = runme3("testing3.dat", "length")
    dev.new()
    point = runme3("testing4.dat", "point")
    dev.new()
    log.jac = runme3("testing5.dat", "log(Jacobian)")

    return (invisible(list(
                           x = log.prior$x,
                           y = log.prior$y,
                           log.prior = log.prior$z,
                           level = level$z,
                           length = len$z,
                           point = point$z,
                           log.jac = log.jac$z)))
}
