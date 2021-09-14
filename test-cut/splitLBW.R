library(INLA)
data <- read.csv("data.final.csv")

#-- Prepare the map --#
library(maptools)
library(sp)
library(spdep)
georgia <- readShapePoly("co13_d00.shp")
data.georgia = attr(georgia, "data")
#################################################
#Create the graph for adjacencies in INLA
#Need the non thinned sph file to do the adjacency matrix!!!
zzz <- poly2nb(georgia)
nb2INLA("Georgia.graph", zzz)
#this create a file called "LDN-INLA.adj" with the graph for INLA
Georgia.adj <- paste(getwd(),"/Georgia.graph",sep="")

#Order based on the map
order <- match(data.georgia$NAME,data[,1])
data<- data[order,]

#--Transform the data to be in the right format for INLA--#
low.vector <- as.vector(as.matrix(data[,2:12]))#by column
E.vector <- as.vector(as.matrix(data[,13:23]))#by column
year <- numeric(0)
for(i in 1:11){ 
  year<- append(year,rep(i,dim(data)[1]))
}
county<- as.factor(rep(data[,1],11))

data<- data.frame(y= low.vector, E= E.vector, ID.area=as.numeric(county), ID.area1=as.numeric(county), year=year,
                  ID.year = year, ID.year1=year, ID.area.year = seq(1,length(county)))

data$intercept <- 1
#############################################################
#--Prepare the model and run inla--#




data$myear <- data$year - mean(data$year)
formula.ST1<- y ~ -1 + intercept + f(ID.area,model="bym",graph=Georgia.adj) + myear




## dput(pLBW.space, file="pLBWspace.txt")

pLBW.time <- inla.cd(debug=TRUE,
                     formula=formula.ST1,
                          data=data,
                          split.by="ID.year",
                          family="poisson",E=data$E,
                          control.predictor=list(link=1),
                          control.inla=list(h=0.05),
                     verbose=FALSE)

## Splitt i rom - dette tar litt lang tid men skal også funke:
if (FALSE) {
    pLBW.space <- inla.cd(formula=formula.ST1,
                          data=data,
                          split.by="ID.area",
                          family="poisson",E=data$E,
                          control.predictor=list(link=1),
                          control.inla=list(h=0.05),
                          verbose=FALSE)
}
