 pac = runif(5)
 phi = inla.ar.pacf2phi(pac)
 pac2 = inla.ar.phi2pacf(phi)
 print("pacf")
 print(pac)
 print("phi")
 print(phi)
 print(paste("Error:", sum(abs(pac2-pac))))
