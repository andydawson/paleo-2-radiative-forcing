library(DBI)
library(neotoma2)
library(sf)
library(dplyr)
library(magrittr)
library(RSQLite)
library(purrr)

#get the neotoma data with all the samples
testt = get_sites(sitename = "St. Clair Lake") %>%
  get_downloads(verbose=TRUE)
taxa(testt)

ageComp = 0
siteidComp = 0

#Put it into a data frame with the selected arguments, use stTestt for complete df
stTest = samples(test)%>%
  filter(elementtype == "pollen" & units == "NISP")%>%
  select(age|agetype|element|taxonid|variablename|depth|siteid|lat|long|elev) %>%
 
stTest

#ageComp = stTest[age] %>%
  siteidComp =  stTest[siteid] %>%
  if (ageComp != stTest[age] & siteidComp != stTest[siteid]){
    relocate(-1,.after = last_col())
    stTest[age] = ageComp
    siteidComp =  stTest[siteid]
  } else if (ageComp != stTest[age]){
    relocate(-1,.after = last_col())
    ageComp = stTest[age]
  }



#sql
mydb <- dbConnect(SQLite(),dbname = "StClair")
dbListTables(mydb)
dbCreateTable(mydb, "tb", stTest)
dbWriteTable(mydb, "StClair", stTest)
dbListTables(mydb)

dbDisconnect(mydb)


#JSON geospatial area
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
            ]
          ]
        ],
        "type": "Polygon"
      }
    }
  ]
}'

#get neotoma data based on json
NaRegion_sf = geojsonsf::geojson_sf(geojson)

loc_data <- get_datasets(NaRegion_sf) %>%
  neotoma2::filter(datasettype == "pollen") %>%
  get_downloads(c(1001))

Alberta = get_sites(1) 
Albertadf = get_datasets(c(1001))

Alberta[[1]]
names(Alberta[[1]])
Alberta[[1]]$sitename

#looking at the data
samples(loc_data)
ncol(loc_data)
loc_data = count(site)
class(loc_data)

#format neotoma into a df
count_by_site <- samples(loc_data) %>%
  filter(elementtype == "pollen") %>%
  select(age|element|taxonid|variablename|depth|siteid|lat|long|elev)
count_by_site
class(count_by_site)
count_by_site%>%count(siteid)










library(tidyverse) # general data wrangling and visualisation
library(pander) # nice tables
library(RRatepol) # rate-of-vegetation change
library(neotoma2) # obtain data from the Neotoma database
library(Bchron) # age-depth modeling
library(janitor) # string cleaning







gl_dataset_download <-
  neotoma2::get_downloads(17334)

# get samples
gl_counts <-
  neotoma2::samples(gl_dataset_download)

# select only "pollen" taxa
gl_taxon_list_selected <-
  neotoma2::taxa(gl_dataset_download) %>%
  dplyr::filter(element == "pollen") %>%
  purrr::pluck("variablename")

# prepare taxa table
gl_counts_selected <-
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




dim(gl_counts_selected)
dim(NARegionIds_tb)
count(NARegionIds_tb[sampleid])

NARegionIds_tb %>% count(depth, siteid) %>%
  dim()

gl_taxon_list_selected

neo_names = colnames(gl_counts_selected)[6:ncol(gl_counts_selected)]
neo_names

rowSums(gl_counts_selected[,6:ncol(gl_counts_selected)])

names(NARegionIds_tb)
head(gl_counts_selected)[, 1:5]




agesNames = c(50, 200, seq(500, 11500, by=500))
N_times = length(ages)
ages_sub = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)
breaks = c(-74, 0.1, 0.35, 0.7, seq(1.2, 11.7, by=0.5))*1000

age_groups <- cut(NATable$age, breaks = breaks, labels = agesNames, right = FALSE)
NATable = cbind(age_groups,NATable)

group_by(NATable$ages, NATable$siteid, )

test <- NATable %>% group_by(siteid, age_groups) %>% summarize(across(8:100,sum))
#ncol(NATable) "python -1"

summarize(sum(across(alnus:picea)))
dim(test)

summary(NATable$age)
NATable$age
age_groups
N_times
ages_sub
breaks

tobe <- c("a","b","c","d","e")
numb <- c(3,4,5)
bythe <- c("f","g","h","d","e")

test = replace(tobe, numb, bythe)
test


















