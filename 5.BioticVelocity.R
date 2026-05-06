library(dplyr)
library(ggplot2)
library(ggpmisc)

#======================================
#load in necessary objects
#===========================================================
BVRough = readRDS("data/pollen_BVs_summary_v5.0.rds")
#albedoDiffs = readRDS("data/albedoDiffs.rds")

#================================================================
#data wrangling
#=====================================================
BVRough$timeFrom = BVRough$timeFrom *-1
BVRough$timeTo = BVRough$timeTo *-1
BVRough$timeMid = BVRough$timeMid *-1

timeMid = c(18525.00, 16545.00, 14565.00, 12585.00, 10605.00, 8625.00, 4665.00, 2685.00, 758.75)
BVs = subset(BVRough, timeMid %in% c(18525.00, 16545.00, 14565.00, 12585.00, 10605.00, 8625.00, 4665.00, 2685.00, 758.75)) 
BVs = BVs[ ,-c(10:21)]

rfMeans = layer_means
midbins = rev(unique(as.numeric(as.character(rfMeans$time))))

BVs$time = midbins[match(BVs$timeMid, timeMid)]

rfMeans$time = as.numeric(as.character(layer_means$time))
testt = inner_join(rfMeans, BVs, by = "time")


ggplot(data = testt,aes(x = bvc_median, y = global)) + geom_point() +
  facet_wrap(~taxon, scales = "free") +
  geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "Radiative Forcing and  Biotic Velocity have some Dependance", x = "Biotic Velocity", y = "Radiative Forcing (W/m^2)") + 
  stat_poly_eq(aes(label = after_stat(rr.label)),formula = y~x,parse = TRUE) #this line to get the r values

ggsave("visuals/BVRF.png", width = 6, height = 4, units = "in", dpi = 300)
