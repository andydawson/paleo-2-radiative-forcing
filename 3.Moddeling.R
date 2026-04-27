library(ggplot2)
library(terra)
library(tidyterra)
library(dplyr)

#============================================================================
#cleans the data and loads in the new stuff
#===================================================================
loadData <- function(){

  rm(list = ls())

  pollenLong = readRDS("Data/pollenLong.rds")
  pollenLongExtra = readRDS("Data/pollenLongExtra.rds")
  pollenWide = readRDS("Data/pollenWide.rds")
  pollenWideExtra = readRDS("Data/pollenWideExtra.rds")

  pbs_ll <- readRDS("pbs_ll.RDS")
  
  tif = rast("blue_sky_monthly_2000-2009.tif") #albedo
  
}

#plot(tif)

#ggplot() +
  #geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey50") +
  #geom_spatraster(data=tif, alpha=0.8) +
  #scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") + 
  #theme_bw() + 
  #theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())+
  #facet_wrap(~lyr)


months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
names(tif) = months
albedoMonth = data.frame(pollenWide$eg)

latlong = data.frame(pollenLong$long,pollenLong$lat)
latlong = distinct(latlong)

bs_df_point = data.frame(matrix(NA, nrow(latlong), 12))
names(bs_df_point) = months

for (i  in 1:12){
  month = months[i]
  bs_df_point[,i] = terra::extract(tif[[month]], latlong, ID = FALSE)
}

sub100 = subset(pollenWide, age_bin == "0")

sub100 = group_by(sub100, siteid)



#make this a for loop to subet all age bins of them and join with bsdfpointt

bs_df_point = cbind(latlong, bs_df_point)
names(bs_df_point) = c("long","lat",months)

modern_albedo = inner_join(sub100, bs_df_point, by = c('lat','long'))

#ggplot() + geom_point(data = modern_albedo, aes(mar,ol))
#ggplot() + geom_point(data = modern_albedo, aes(mar,eg))
#ggplot() + geom_point(data = modern_albedo, aes(mar,sg))

#===============================================================================
# run the moddels to fit albedo to the data
#===============================================================================
modelAlbedo <- function(modern_albedo,modNum){
library(mgcv)

  if (modNum == 1){
mod1 = mgcv::bam(mar ~ s(lat, long, bs="gp", k=10) + s(ol, k=10),
                 data=modern_albedo, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit)
gam.check(mod1)
saveRDS(mod1,"models/mod1.rds")
  }
  if (modNum == 2){
#k = 100 x 1
mod2 = mgcv::bam(mar ~ s(lat, long, bs="gp", k=100) + s(ol, k=10),
                 data=modern_albedo, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit)
gam.check(mod2)
saveRDS(mod2,"models/mod2.rds")
}
if (modNum == 3){
#k = 100 x2
mod3 = mgcv::bam(mar ~ s(lat, long, bs="gp", k=100) + s(ol, k=100),
                 data=modern_albedo, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit)
gam.check(mod3)
saveRDS(mod3,"models/mod3.rds")
}
  if (modNum == 4){
#summergreen
mod4 = mgcv::bam(mar ~ s(lat, long, bs="gp", k=100) + s(sg, k=100),
                 data=modern_albedo, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit)
gam.check(mod4)
saveRDS(mod4,"models/mod4.rds")
  }
  if (modNum == 5){
#evergreen
mod5 = mgcv::bam(mar ~ s(lat, long, bs="gp", k=100) + s(eg, k=100),
                 data=modern_albedo, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit)
gam.check(mod5)
saveRDS(mod5,"models/mod5.rds")
  }
  if (modNum == 6){

mod6 = mgcv::bam(mar ~ s(lat, long, bs="gp", k=100) + s(ol, sg, eg, k=100),
                 data=modern_albedo, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit)
gam.check(mod6)
saveRDS(mod6,"models/mod6.rds")
  }
  #return(mod+modNum)
}

mod6 = modelAlbedo(modern_albedo, 6)

cal_interp_model = readRDS(paste0('models/mod6.rds'))
foo = predict.gam(cal_interp_model, 
            newdata = modern_albedo , 
            type    = 'response')

#sanity check look further into 1:1 of the model and modern albedo
ggplot() + geom_point(aes(x = modern_albedo$mar, y = foo)) +
  geom_abline(show.legend = TRUE)


#predict
#cal_interp_model = readRDS(paste0('models/mod6.rds'))

print(">>Predict")
paleo_interp_predict_gam_vec = predict.gam(cal_interp_model, 
                                           newdata = pollenWideExtra , 
                                           type    = 'response')
paleo_interp_predict_gam = data.frame(pollenWideExtra,
                                    alb_mean = paleo_interp_predict_gam_vec)
saveRDS(paleo_interp_predict_gam, "models/predictMar.rds")
write.csv(paleo_interp_predict_gam, "models/predictMar.csv")



#take differences of albedo mean at each age bin change
albedo_bin_diff <- function(df, ordered_bins = c("0", "5", "10", "15", "20", "25")) {
  # --- Compute consecutive differences per site --------------
  result <- df %>%
    dplyr::select(siteid, lat, long, age_bin, alb_mean,) %>%
    arrange(siteid,lat, long,age_bin) %>%
    group_by(siteid) %>%
    mutate(
      from_bin = dplyr::lag(age_bin),
      alb_from = dplyr::lag(alb_mean),
      to_bin   = age_bin,
      alb_to   = alb_mean,
      alb_diff = alb_to - alb_from        # NA if either side is NA
    ) %>%
    ungroup() %>%
    filter(!is.na(from_bin)) %>%
    dplyr::select(siteid,lat, long,from_bin, to_bin, alb_from, alb_to, alb_diff)
  
  return(result)
}

albedoDiffs = albedo_bin_diff(paleo_interp_predict_gam)
#foo = albedoDiffs[!is.na(albedoDiffs$alb_to) &!is.na(albedoDiffs$alb_from)  ,]


albedoDiffs$long360 = 180 + 180 - abs(albedoDiffs$long)
albedoDiffs$lat180 = albedoDiffs$lat + 90

# create spatial object of unique coordingates
ll_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

library(raster)
library(ncdf4)

alb_diff_spatial = SpatialPointsDataFrame(coords = albedoDiffs[,c('long360', 'lat')], 
                                          data = albedoDiffs, 
                                          proj4string = CRS(ll_proj))





rk_hadgem = brick('HadGEM3-GA7.1_TOA_kernel_L19.nc', level=1, varname='albedo_sw_cs')
rk_hadgem_month = raster::extract(rk_hadgem[[3]], alb_diff_spatial)
albedoDiffs$rk_hadgem = rk_hadgem_month

albedoDiffs$rf = albedoDiffs$alb_diff * albedoDiffs$rk_hadgem * 100


ggplot(albedoDiffs) +geom_boxplot(aes(x=from_bin, y =rf)) 
ggplot(albedoDiffs) +geom_point(aes(x=long,y=lat,color = rf))+facet_wrap(~from_bin) + scale_colour_distiller(type = "div", palette = "PuOr",na.value = NA)
ggplot(pollenWideExtra) + geom_point(aes(x=long,y=lat,color = ol))+facet_wrap(~age_bin)
ggplot(paleo_interp_predict_gam) + geom_point(aes(x=long,y=lat,color = alb_mean))+facet_wrap(~age_bin)



#find why there is missing values in albedoDiffs

# create spatial object of unique coordingates

ggplot(foo) +geom_boxplot(aes(x=from_bin, y =rf )) 
ggplot(foo) +geom_point(aes(x=long,y=lat,color = rf))+facet_wrap(~from_bin) + scale_colour_distiller(type = "div", palette = "PuOr")
ggplot(pollenWide) + geom_point(aes(x=long,y=lat,color = ol))+facet_wrap(~age_bin)


#sanity check look further into 1:1 of the model and modern albedo
#maps of calibration
#map of landcover
#maps of albedo, albedo difs
#map of rf, specific time intervals
#rf, rf kernal
#

library(ebirdst)

assign_to_grid(albedoDiffs, coords = c("lat", "long"), is_lonlat = TRUE, res = c(200000, 140000))
library(terra)
library(tidyterra)
library(ggplot2)
#remotes::install_github("dieghernan/tidyterra")

test = subset(albedoDiffs, subset = !is.na(rf))

# Create spatial points (vect)
v <- vect(test, geom=c("long", "lat"), crs="+proj=longlat +datum=WGS84")


# Rasterize
r <- rast(v, res=1) # sets resolution to 1 degree
r <- rasterize(v, r, field="rf", fun = mean, by = 'from_bin')
image(r)
ggplot() + tidyterra::geom_spatraster(data = r) +facet_wrap(~lyr) + scale_fill_distiller(type = "div", palette = "PuOr", values=c(0,0.2, 0.8,1))

#work on map this map ^^^^
#summarize boxplot on spatraster?
#other visuals, land cover moving througb time
#push code to github
#continue working on presentation

#simulate
print(">>Simulate")
paleo_interp_sim_gam = simulate(cal_interp_model,
                                nsim = 100,
                                data = albedoDiffs)

paleo_interp_sim_gam_df = data.frame(pollen_by_group_wide,  
                                     paleo_interp_sim_gam)
