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

load("Rs")
g <- inla.read.graph(Rs)

formula <- O ~ f(ID.area, model = "bym", graph = g, constr = TRUE)
load("data.INLA")
rr <- inla(formula, data = data.INLA,
           family = "poisson", E = E,
           control.compute = list(control.gcpo = list(enable = T, num.level.sets = 10)), 
           verbose = T, inla.call = "", keep = T)

r1 <- inla(formula, data = data.INLA,
           family = "poisson", E = E,
           control.compute = list(config = TRUE), 
           verbose = !TRUE)

r2 <- inla(formula, data = data.INLA,
           family = "poisson", E = E,
           control.compute = list(config = TRUE), 
           verbose = !TRUE, inla.call = "inla.mkl.work")
