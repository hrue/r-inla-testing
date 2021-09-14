wd = './wd & wd'

r = inla(y ~ 1, data = data.frame(y = 1:10),
         keep = TRUE,
         working.directory = wd,
         verbose = TRUE)


