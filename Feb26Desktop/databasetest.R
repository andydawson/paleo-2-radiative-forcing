library(DBI)
library(neotoma2)
library(sf)
library(dplyr)
library(magrittr)
library(RSQLite)

#get the neotoma data with all the samples
testt = get_sites(sitename = "St. Clair Lake") %>%
  get_downloads(verbose=TRUE)

#Put it into a data frame with the selected arguments, use stTestt for complete df
stTest = samples(test)%>%
  filter(elementtype == "pollen" & units == "NISP")%>%
  select(age|agetype|element|taxonid|variablename|depth|siteid|lat|long|elev)
stTest

#sql
mydb <- dbConnect(SQLite(),dbname = "StClair")
dbListTables(mydb)
dbCreateTable(mydb, "ta", stTestt)
dbWriteTable(mydb, "tc", stTest)
dbListTables(mydb)
dbReadTable(mydb, "ta")



dbDisconnect(mydb)

