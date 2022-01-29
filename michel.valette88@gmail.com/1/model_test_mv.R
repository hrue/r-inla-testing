########### Define the deforestation function #################################
############## Spatio-temporal model ###########################################
################################################################################



library(INLA)
library(sf)
library(sp)
library(inlabru)
library(raster)
inla.setOption(inla.mode="experimental") 
bru_options_set(inla.mode = "experimental")
load("data_model.RData")


### Function to have value of covariate with inlabru 
f.mcwd<-function(x,y,year){
  output<-vector()
  spp <- SpatialPoints(data.frame(x = x, y = y), proj4string = fm_sp_get_crs(mcwd2))
  spp$year<-year ### Change in the function 
  proj4string(spp) <- fm_sp_get_crs(mcwd2)
  
  ### Extract the value 
  v<-over(spp,mcwd2,main.layer=year)
  for (i in 1:nrow(v)){
    output[i]<-v[i,year[i]]
  }
  output[is.na(output)]<-0
  output<-scale(output,scale=TRUE,center=TRUE)
  return(output)}


formula.1 <- coordinates+year ~
  #mcwd(f.mcwd(x,y,year),model="linear")+
  mySPDE(main=coordinates,model = matern, group = year, ngroup = nyear, control.group=list(model="ar1"))






fit1 <- lgcp(formula.1, modis2,ips,domain=list(coordinates=mesh,year=modis2$year),options = list(safe=TRUE, inla.mode="experimental"))


fit1$summary.fixed
fit1$dic$dic



