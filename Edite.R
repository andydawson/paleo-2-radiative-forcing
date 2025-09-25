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
translator <- read.csv("C:/Users/Bacon/OneDrive/Documents/RA/transTable.csv")

#gets positions of each classification
posTranslator = match(names(MasterNATable), translator[,1],nomatch = NA_integer_, incomparables = names(NATable[1:8]))
posTranslator = posTranslator[-c(1:8)]

#hold for translator cause replace didnt like the translator[,2]
namesTranslator <- translator[,2]

#temp names to be added later
tempNames <- names(NATable)
tempNames <- tempNames[-c(1:8)]

#before the translated df
preLCT = replace(tempNames,posTranslator, namesTranslator)

#temp hold these columns,       WHAT ARE THEY, DOCCUMENT BETTER INSTEAD OF BEING ON CAFFINNE
hold = NATable[2:8]

#temp hold these other columsn and change their name
hold1 = NATable[7:length(NATable)]
colnames(hold1) = preLCT

#the newly translated table
LCT = data.frame(c(hold,hold1))

#sort through the LCT and collect every column based on evergreen, summergreen, openland, and NA
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

LCTSum = data.frame(c(hold, evergreens, summergreens, openland))
LCTSum <- LCTSum  %>%
  rename_at(8, ~"evergreen") %>%
  rename_at(9, ~"summergreen") %>%
  rename_at(10, ~"openland") 

LCTSum = cbind(LCTSum,TableSum)
LCTSum <- LCTSum[-c(8,9,10)]

LCTSum <- LCTSum[!is.na(LCTSum$age),]
LCTSum <- LCTSum[!LCTSum$age > 25000,]

LCTSum <- LCTSum %>%
  mutate(MasterNATable$"lat",MasterNATable$"long", .after = 'age')

breaks = c(0,5000,10000,15000,20000,25000)
breaklabels = c("0","5","10","15","20")
print(table(cut(LCTSum$age,breaks ,labels = breaklabels)))
print(class(max(LCTSum$age)))

LCTSum = cbind(LCTSum,AgeBins = cut(LCTSum$age,breaks ,labels = breaklabels))




Bin = c()
MasterBin = c()

#This function is used for calculating the sum of the land types in age bins
BinMaker <- function(ya = 10,i = 3){
  ###This function is to make bins with a year catagory 
  ###and sums the three land types
  
  Bin = append(Bin,ya)
  Bin = append(Bin,LCTSum[i,"siteid"])
  Bin = append(Bin, as.numeric(LCTSum[i,'summergreen'])) %>%
    + Bin[3]
  Bin = append(Bin, as.numeric(LCTSum[i,'evergreen'])) %>%
    + Bin[4]
  Bin = append(Bin, as.numeric(LCTSum[i,'openland'])) %>%
    + Bin[5]
  print(Bin)
  
  MasterBin = append(MasterBin,Bin)
  return(MasterBin)
  print("returned something?")
  #Bin_ya = c(LCTSum[idxLen,'evergreen'], LCTSum[idxLen,'summergreen'], LCTSum[idxLen,'openland']) %>%
   # + Bin_ya #ya has to be the variable
}


x = BinMaker()
x



#inde3xer
idxLen = 1
Bin_ya = data.frame(
  AgeBins = c()
)
i = 0
print(LCTSum[1,"age"] < 5000)
for (i in LCTSum$age){
  print(i)
  if (LCTSum[i,"age"] < 5000){
    if (LCTSum[i,"siteid"] == LCTSum[i+1, "siteid"]){
      Bin_ya = Bin_ya %>%
        c(LCTSum[i,"siteid"],LCTSum[i,"5000"],LCTSum[i,"evergreen"],LCTSum[i,"summergreen"],LCTSum[i,"openland"]) #%>% #LCTSum[i,"lat"],LCTSum[i,"long"],
        #c(LCTSum[i,"evergreen"]+Bin_ya["evergreen"]) %>%
        #c(LCTSum[i,"sumergreen"]+Bin_ya["summergreen"]) %>%
        #c(LCTSum[i,"openland"]+Bin_ya["openland"])
    }
  }
  else{
  }
}




sum_by_age_bin <- function(df, age_breaks, col_names = c("col1", "col2", "col3")) {
  # Check if required columns exist
  required_cols <- c("id", "age", col_names)
  if (!all(required_cols %in% names(df))) {
    stop("Data frame must contain: id, age, and the specified columns.")
  }
  
  # Create age bins
  df$age_bin <- cut(df$age, breaks = age_breaks, right = FALSE, include.lowest = TRUE)
  
  # Sum the specified columns by age_bin
  summed_df <- aggregate(df[, col_names], by = list(AgeBin = df$age_bin), FUN = sum, na.rm = TRUE)
  
  # Add list of IDs per age bin
  id_list <- aggregate(df$id, by = list(AgeBin = df$age_bin), FUN = function(x) paste(unique(x), collapse = ","))
  names(id_list)[2] <- "ids"
  
  # Merge sum and ids
  result_df <- merge(summed_df, id_list, by = "AgeBin")
  
  return(result_df)
}
ageBins = c(5000,10000,15000,20000,25000)
sum_by_age_bin(LCTSum, ageBins, cols = c(LCTSum[evergreen], LCTSum[summergreen], LCTSum[openland]))
#loops through the age column and makes the ages into bins based on LCT
#while (idxLen < length(LCTSum$age)){
#  
#  print(idxLen)
#  siteid1 = as.numeric(LCTSum[idxLen,"siteid"])
#  siteid2 = as.numeric(LCTSum[idxLen+1,"siteid"])

#  if (siteid2 != siteid1) {
#    #CTSum5 = cbind(LCTSum[idxLen,1:4], CTSum5) #this needs to be editied
#    print("made it into the loop, FALSE")
#    print(idxLen)
#  } 
#  
#  else if (siteid1 == siteid2){
#    if (LCTSum[idxLen, 'age'] < 5000){
#      print("made it into the loop, 5")
#      print(LCTSum[idxLen,'evergreen'])
#      #Bin_ya = Bin_ya + c(cbind(LCTSum[idxLen,'evergreen'],2), cbind(LCTSum[idxLen,'summergreen'],3), cbind(LCTSum[idxLen,'openland']),4) 
#    } 
    
#    else if (LCTSum[idxLen, 'age'] < 10000 & (LCTSum[idxLen, 'age'] > 5000)){
#      print("made it into the loop, 10")
#      Bin_ya = c(LCTSum[idxLen,'evergreen'], LCTSum[idxLen,'summergreen'], LCTSum[idxLen,'openland']) %>%
#        + Bin_ya #ya has to be the variable
#    }
#    
#    else if (LCTSum[idxLen, 'age'] > 10000 & (LCTSum[idxLen, 'age'] < 15000)){
#      Bin_ya = c(LCTSum[idxLen,'evergreen'], LCTSum[idxLen,'summergreen'], LCTSum[idxLen,'openland']) %>%
#        + Bin_ya
#    }
#    
#    else if (LCTSum[idxLen, 'age'] > 15000 & (LCTSum[idxLen, 'age'] < 20000)){
#      Bin_ya = c(LCTSum[idxLen,'evergreen'], LCTSum[idxLen,'summergreen'], LCTSum[idxLen,'openland']) %>%
#        + Bin_ya
#    }
#    
#    else if (LCTSum[idxLen, 'age'] > 20000 & (LCTSum[idxLen, 'age'] < 25000)){
#      Bin_ya = c(LCTSum[idxLen,'evergreen'], LCTSum[idxLen,'summergreen'], LCTSum[idxLen,'openland']) %>%
#        + Bin_ya
#    }
#  }
  
#  idxLen = idxLen + 1
#  print("indexed")
#} 




#why?
TableSum <- 
  rowSums(LCTSum[,6:8])

#add in sum columns and make a percent table
LCTPerc <-
  signif(LCTSum[6:8]/TableSum,2)

LCTPerc <- cbind(LCTSum[1:5],LCTPerc)
  

#Help functions
#
#neotoma2::get_table('datasettypes')
#pingNeotoma(server = "neotoma")
#dim(NATablePerc)

