inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(inla.mode = "experimental")
inla.setOption(num.threads = "1:1")

system("make -B")
n <- 5
##cmodel <- inla.cgeneric.define(model = "inla_cgeneric_ar1_model", shlib = "cgeneric-demo.so",n = n)
cmodel <- inla.cgeneric.define(model = "inla_cgeneric_iid_model", shlib = "cgeneric-demo.so",n = n)
inla.cgeneric.q(cmodel)


