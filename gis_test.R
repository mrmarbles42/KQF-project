library(maptools)
library(sp)
require(raster)
require(maps)
require(ggmap)
require(marmap)
require(lattice)


data("wrld_simpl")
plot(wrld_simpl)

xlim=c(-130,-60)
ylim=c(20,60)
plot(wrld_simpl,
     xlim=xlim,
     ylim=ylim,
     col = "olivedrab3",
     bg = "lightblue")
