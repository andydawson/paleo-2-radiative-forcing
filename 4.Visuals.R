library(ggplot2)
library(tidyterra)
library(RColorBrewer)
library(ggpmisc)
library(sf)

#=============================================================
#Load in necessary files
#=========================================================
pbs_ll = readRDS("pbs_ll_good.rds")
ice = readRDS('data/glacier_shapefiles_21-1k.RDS')
ice_years = seq(1, 21)*1000

ice_fort = data.frame(matrix(NA, nrow=0, ncol=9))
ages_sub = unique(as.numeric(as.character(pollenWide$age_bin)))
for (i in 1:length(ages_sub)){
  print(i)
  idx_ice_match = which.min(abs(ages_sub[i] - ice_years))
  
  #proj4string(ice[[idx_ice_match]]) = proj_WGS84
  #ice[[idx_ice_match]] = spTransform(ice[[idx_ice_match]], CRS(proj_WGS84))
  
  ice_fort_age = fortify(ice[[idx_ice_match]])
  
  ice_fort = rbind(ice_fort,
                   data.frame(ice_fort_age,
                              ice_year = rep(ice_years[idx_ice_match], nrow(ice_fort_age)),
                              ages = rep(ages_sub[i], nrow(ice_fort_age))))  
}

ice_sub = ice_fort[which(ice_fort$ages %in% ages_sub),]

ice_sub$lyr = factor(ice_sub$ages,
                        levels = c('0', '50','500', '2000', '4000', '8000', '10000', '12000', '14000','16000','18000','20000'))


ggplot() + geom_polygon(data = ice_sub, aes(x = long, y = lat, group = group)) +
  facet_wrap(~lyr)
saveRDS(ice_sub, 'data/ice_sub.RDS')
#=========================================================
# NA maps and sites
#=========================================================
ggplot() +
  geom_point(data = pbs_ll, aes(x = long, y = lat, color = "PBS Locations"), alpha = 0.8, size = 0.5, colour = "gray12") +
  geom_point(data = pollenWide, aes(long, lat, group = siteid, color = "Pollen Sites"), colour ="steelblue") +
  #scale_color_manual(name = "", values = c("Pollen Sites" = "steelblue", "North America" = "gray12")) +
  labs(title = "Pollen Site Locations in North America",x = "Longitude",y = "Latitude") +
  theme_void()

ggsave("visuals/Map_Sites.png", width = 6, height = 4, units = "in", dpi = 300)
#=========================================================
# Model accuracy
#=========================================================

ggplot() +
  geom_point(aes(x = modern_albedo$mar, y = foo), color = "steelblue", alpha = 0.7) +
  geom_smooth(aes(x = modern_albedo$mar, y = foo, linetype = "Fitted Line"), method = "lm", color = "black",se = TRUE,show.legend = TRUE) +
  stat_poly_eq(aes(x = modern_albedo$mar, y = foo,
                   label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               method = "lm", parse = TRUE, size = 3.5,
               label.x = "left", label.y = "top") +
  scale_linetype_manual(name = "", values = c("Fitted Line" = "dashed")) +
  labs(title = "Model Accuracy: Predicted vs. Observed March Albedo ", x = "Observed March Albedo ", y = "Predicted Albedo") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("visuals/Scatter_Model.png", width = 6, height = 4, units = "in", dpi = 300)

#=========================================================
# RF boxplots
#=========================================================
ggplot(albedoDiffs) +
  geom_boxplot(aes(x = from_bin, y = rf, fill = from_bin)) +
  stat_summary(aes(x = from_bin, y = rf, label = after_stat(round(y, 2))),fun = median,geom = "text",vjust = -0.5,size = 3) +
  labs(title = "Radiative Forcing by Albedo Change Bin", x = "Years from Present", y = "Radiative Forcing (W/m^2) ", fill = "Albedo Bin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")

ggsave("visuals/Box_RF.png", width = 6, height = 4, units = "in", dpi = 300)

#=========================================================
# rf raster
#=========================================================

#image(r)
r_subset <- r[[c("0","8000", "10000", "14000")]]
ice_subset = ice_sub[ice_sub$lyr %in%c("0","8000", "10000", "14000"),]
ggplot() +
  geom_point(data = pbs_ll, aes(x = long, y = lat), color = "gray12", alpha = 0.8, size = 0.2) +
  tidyterra::geom_spatraster(data = r_subset) + #reomive N and W ticks
  geom_polygon(data = ice_subset, aes(x = long, y = lat, group = group), color = 'darkslateblue', fill = 'darkslateblue', alpha = 0.6) +
  facet_wrap(~lyr) +
  scale_fill_distiller( type = "div",palette = "RdBu", values = c(0, 0.25, 0.45, 0.55,0.75, 1), name = "Radiative Forcing (W/m^2)", na.value = "transparent") +
  labs(title = "Radiative Forcing from the LGM", x = "Longitude", y = "ya") +
  theme_void() +
  theme(legend.position = "right", strip.text = element_text(face = "bold"))

ggsave("visuals/ya.png", width = 6, height = 4, units = "in", dpi = 300)

#=========================================================
#bar plot, means of rf
#=========================================================
layer_means$range = c("0.05-0", "0.5-0.05", "2-0.5", "4-2", "6-4", "8-6", "10-8", "12-10", "14-12", "16-14", "18-16", "20-18")
ggplot() +geom_bar(data = layer_means, aes(x = global, y = time, fill = global), stat = "identity") +
  geom_text(data = layer_means, aes(x = global, y = time,label = round(global, 2),hjust = ifelse(global >= 0, -0.2, 1.2)),size = 3) +
  scale_y_discrete(limits=rev) + 
  scale_x_continuous(limits = c(-0.22, 0.22)) +
  labs(title = "Median Radiative Forcing from the Holoscene", x = 'Median Radiative Forcing (W/m^2)', 
       y = 'Years Before Present') +
  scale_fill_gradient2(high = "firebrick", mid = "grey94", low = "blue4",midpoint = 0,name = "Radiative Forcing")+
  geom_vline(xintercept = 0, color = "gray30", linewidth = 0.5) +
  labs(title = "Median Radiative Forcing from the Holocene",x = "Median Radiative Forcing",y = "Years Before Present") +
  theme_bw()

ggsave("visuals/Bar_RF.png", width = 6, height = 4, units = "in", dpi = 300)

#=========================================================
#hist of rf, cutoff at 5 and -5?
#=========================================================

ggplot()+
  geom_histogram(data = albedoDiffs, aes(x = rf))

#=========================================================
#lnad cover type maps
#=========================================================

pleasework <- pollenWideExtra %>%
  filter(!is.na(eg), !is.na(sg), !is.na(ol)) %>%
  mutate(tile_color = rgb(eg, ol, sg))

ggplot() +
  geom_point(data = pbs_ll, aes(x = long, y = lat), alpha = 0.8, size = 0.5, color = "black") +
  geom_tile(data = pleasework, aes(x = long, y = lat, fill = tile_color), width = 1, height = 1) +
  scale_fill_identity(guide = "legend",labels = c("Evergreen", "Open Land", "Summergreen"),breaks = c(rgb(1,0,0), rgb(0,1,0), rgb(0,0,1))) +
  labs(title = "North America Land Cover", subtitle = "From present to 20,000 ya",fill = "Land Cover Type") +
  facet_wrap(~age_bin) +
  theme_void() 
#geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +

#=============================================================
#albedo maps back through time
#-=================================================================

