library(ggplot2)
library(terra)

blueSky = rast("H:/RA/blue_sky_monthly_2000-2009.tif")
#FOR EACH POLEN RECORD, WE ONLY WANT ONE SET OF COUNTS
subs = subset(MasterNATable, age < 100)
#
coords = subs[,c('long', 'lat','pinus','alnus')]
vec = vect(coords, geom = c("long","lat"), crs = "EPSG:4326")

tbl = extract(blueSky,vec)

#TO THINK ABOUT
r_template <- rast(vec, resolution=0.5) # Define an
r_from_points <- rasterize(vec, r_template, field="pinus", fun=mean)

plot(r_from_points)
