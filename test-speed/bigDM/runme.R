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
url <- "https://emi-sstcdapp.unavarra.es/bigDM/inst/csv/data.csv"
data <- read.csv(url)

##debug(CAR_INLA)
##Global <- CAR_INLA(carto=Carto_SpainMUN, ID.area="ID", O="obs", E="exp", model="global")

load("Rs")
g <- inla.read.graph(Rs)

formula <- O ~ f(ID.area, model = "bym2", graph = g, constr = TRUE)
load("data.INLA")
r <- inla(formula, data = data.INLA,
          family = "poisson", E = E,
          inla.mode = "experimental",
          ##keep = TRUE,
          verbose = TRUE)

