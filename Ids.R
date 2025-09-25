library(neotoma2) #The Neotoma Database package
library(dplyr) #do i need to explain?
library(tidyverse) # general data wrangling and visualisation
library(pander) # nice tables
#library(RRatepol) # rate-of-vegetation change
library(Bchron) # age-depth modeling
library(janitor) # string cleaning
library(ggplot2)

#This script gets you the Neotoma Pollen Data and makes both a Master File containing sample ids, Element type,
# age, age, depth, site ids, and taxon with their counts. 
#
#NARegionsIds_sf = The raw Neotoma downloads
#NARegionIDs_tb = The uneditied Neotoma downloads in table format
#MasterNATable = The editied Neotoma Table for use DO NOT CHANGE 

#make polygon of 
geojson = '{
  "type": "FeatureCollection",
  "features": [
    {
      "type": "Feature",
      "properties": {},
      "geometry": {
        "type": "Polygon",
        "coordinates": [
          [
            [
              -169,
              24
            ],
            [
              -169,
              75
            ],
            [
              -52,
              75
            ],
            [
              -52,
              24
            ],
            [
              -169,
              24]]]}}]}'


#get region bound in neotoma usable format
NaRegion_sf = geojsonsf::geojson_sf(geojson)

countries = c("Canada, United States")

#get a data frame of all the dataset ids in region
NARegionIds_sf <- get_sites(gpid = "Alberta", all_data = TRUE) %>%
  neotoma2::filter(datasettype == "pollen") %>%
  get_downloads()

#get a data frame of all the dataset ids in region
#NARegionIds_Us <- get_sites(gpid = "United States", all_data = TRUE) %>%
#  neotoma2::filter(datasettype == "pollen") %>%
#  get_downloads()

# select only "pollen" taxa
taxa(NARegionIds_sf)

NARegionTaxa <-
  neotoma2::taxa(NARegionIds_sf) %>%
  dplyr::filter(element == "pollen") %>%
  #select(taxonid|element|ages|variablename|depth|siteid|lat|long|elev) %>%
  purrr::pluck("variablename")

#make a table of the dataframe for usable data DO NOT EDIT THE MASTER TABLE
MasterNATableA <-
  NARegionIds_sf %>%
  get_downloads(all_data = TRUE) %>%
  as.data.frame() %>%
  dplyr::mutate(sample_id = as.character(siteid)) %>%
  tibble::as_tibble() #%>%
  dplyr::select("sample_id", "sitename","siteid","ages","element","lat","long") %>%
  # only include pollen
  dplyr::filter(
    element == "pollen" 
  ) %>%
  # turn into the wider format
  tidyr::pivot_wider(
    names_from = "variablename",
    values_from = "value",
    values_fill = 0
  ) %>%
  # clean names
  janitor::clean_names()
names(NARegionIds_sf)


ggplot(data = MasterNATable) + geom_point(aes(x=long, y=lat)) + geom_polygon(data=pbs_ll, aes(x=long, y=lat, group=group))

pbs_ll <- readRDS("D:/RA/pbs_ll.RDS")
ggplot() + geom_polygon(data=pbs_ll, aes(long, lat, group=group)) +geom_point(data=MasterNATable, aes(x=long, y=lat, color = alnus))



  #Help functions
#
#neotoma2::get_table('datasettypes')
#pingNeotoma(server = "neotoma")
#dim(NATablePerc)
