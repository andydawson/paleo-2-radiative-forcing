library(neotoma2) #The Neotoma Database package
library(dplyr) #do i need to explain?
library(tidyverse) # general data wrangling and visualisation
library(pander) # nice tables
library(RRatepol) # rate-of-vegetation change
library(Bchron) # age-depth modeling
library(janitor) # string cleaning

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
        "coordinates": [
          [
            [
              -138.3341229586096,
              62.0988743154773
            ],
            [
              -128.0361829807769,
              45.59133244143436
            ],
            [
              -99.20886211455341,
              47.45476393714668
            ],
            [
              -97.75131048276042,
              62.00281248141573
            ],
            [
              -138.3341229586096,
              62.0988743154773
            ]]], "type": "Polygon"} }]}'

#{
#"type": "FeatureCollection",
#"features": [
#  {
#    "type": "Feature",
#    "properties": {},
#    "geometry": {
#      "type": "Polygon",
#      "coordinates": [
#        [
#          [
#            -169,
#            24
#          ],
#          [
#            -169,
#            75
#          ],
#          [
#            -52,
#            75
#          ],
#          [
#            -52,
#            24
#          ],
#          [
#            -169,
#            24
#          ]
#        ]
#      ]
#    }
#  }
#]
#}


#get region bound in neotoma usable format
NaRegion_sf = geojsonsf::geojson_sf(geojson)

#get a data frame of all the dataset ids in region
NARegionIds_sf <- get_sites(gpid = "Alberta", all_data = TRUE) %>%
  neotoma2::filter(datasettype == "pollen") %>%
  get_downloads()

# select only "pollen" taxa
NARegionTaxa <-
  neotoma2::taxa(NARegionIds_sf) %>%
  dplyr::filter(element == "pollen") %>%
  purrr::pluck("variablename")

#make a table of the dataframe for usable data DO NOT EDIT THE MASTER TABLE
MasterNATable <-
  NARegionIds_tb %>%
  as.data.frame() %>%
  dplyr::mutate(sample_id = as.character(sampleid)) %>%
  tibble::as_tibble() %>%
  dplyr::select("sample_id", "value", "variablename","depth","siteid","age","element") %>%
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


#Help functions
#
#neotoma2::get_table('datasettypes')
#pingNeotoma(server = "neotoma")
#dim(NATablePerc)
