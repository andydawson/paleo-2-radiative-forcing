library(dplyr)
library(qdapTools)
library(tidyr)
#This script also makes a table filled with the percent of each taxon at each date 
#along with their land cover type
###################################################################################################################################
#NATable = The table that can be editied
#LCTSum = the tbale of the three land cover options summarized
#LCTPerc = The table but in percents



#Translate Taxon to the three types
#reads csv file and makes it into a matrix
translator <- read.csv("C:/Users/Jonah/Documents/R Scripts/transTable.csv")

#gets positions of each classification
posTranslator = match(names(NATable), translator[,1],nomatch = NA_integer_, incomparables = names(NATable[1:6]))
posTranslator = posTranslator[-c(1:6)]

#hold for translator cause replace didnt like the translator[,2]
namesTranslator <- translator[,2]

#temp names to be added later
tempNames <- names(NATable)
tempNames <- tempNames[-c(1:6)]

#before the translated df
preLCT = replace(tempNames,posTranslator, namesTranslator)

#temp hold these columns
hold = NATable[2:6]

#temp hold these other columsn and change their name
hold1 = NATable[7:length(NATable)]
colnames(hold1) = preLCT

#the newly translated table
LCT = data.frame(c(hold,hold1))

#sort through the LCT and sum each classification
for (i in colnames(LCT)) {
  if (grepl("eg", colnames(LCT[i]), fixed = FALSE))
  {
    evergreens <- LCT[i] + evergreens
  } else if (grepl("sg", colnames(LCT[i]), fixed = FALSE))
  {
    summergreens <- LCT[i] + summergreens
  } else if (grepl("ol", colnames(LCT[i]), fixed = FALSE))
  {
    openland <- LCT[i] + openland
  } else if (grepl("na", colnames(LCT[i]), fixed = FALSE))
  {
    notapplicable <- LCT[i] + notapplicable
  }
}

#evergreens = 0
#summergreens = 0
#openland = 0
#notapplicable = 0

LCTSum = data.frame(c(hold, evergreens, summergreens, openland))
LCTSum <- LCTSum  %>%
  rename_at(6, ~"evergreen") %>%
  rename_at(7, ~"summergreen") %>%
  rename_at(8, ~"openland") 


#add in sum columns and make a percent table
TableSum <- 
  rowSums(LCTSum[,6:8])

LCTSum = cbind(LCTSum,TableSum)

LCTPerc <-
  signif(LCTSum[6:8]/TableSum,2)

LCTPerc <- cbind(LCTSum[1:5],LCTPerc)

#Help functions
#
#neotoma2::get_table('datasettypes')
#pingNeotoma(server = "neotoma")
#dim(NATablePerc)

