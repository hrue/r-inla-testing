library(INLA)
library(bigDM)
library(sf)
temp <- tempfile()
filename <- paste(temp,"zip",sep=".")
if (!file.exists("Carto_SpainMUN.zip")) {
    download.file("https://emi-sstcdapp.unavarra.es/bigDM/inst/shape/Carto_SpainMUN.zip","Carto_SpainMUN.zip")
}
unzip("Carto_SpainMUN.zip",exdir=temp)
carto_sf <- st_read(temp)
unlink(temp, recursive = TRUE)
##url <- "https://emi-sstcdapp.unavarra.es/bigDM/inst/csv/data.csv"
##data <- read.csv(url)
data <- read.csv("data.csv")

##INLA:::inla.my.update()

load("Rs")
g <- inla.read.graph(Rs)

formula <- O ~ f(ID.area, model = "bym", graph = g, constr = TRUE)
load("data.INLA")
rr <- inla(formula,
           data = rbind(data.INLA, data.INLA, data.INLA, data.INLA), 
           family = "poisson", E = E,
           control.compute = list(control.gcpo = list(enable = FALSE, num.level.sets = 10)), 
           num.threads = "1:1", 
           verbose = T)
summary(rr)
rr$cpu.used
