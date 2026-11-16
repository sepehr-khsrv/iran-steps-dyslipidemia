library(haven)
steps86 <- read_dta("../data_raw/2007/stps-Total86 - Original.dta")
####weight for steps 86 
library(readxl)
library(haven)
library(tidyverse)
library(dplyr)
library(writexl)
library(stringr)
steps86$age <- 86 - steps86$c201
steps86$partici3 <- steps86$i1
steps86$province <- steps86$i4particip

##################################################################
#create data frame with 2 rows and nrow columns
df <- data.frame(matrix(ncol = 2, nrow = nrow((steps86))))

#provide column names
colnames(df) <- c('Year', 'Ref')
df$Year="86"
df$Ref <- 1:nrow(df) 
steps86 <- cbind(df,steps86)
write_xlsx(steps86,"../data_raw/2007/steps86long.xlsx")
##################################################################

# Create New DataFrame of Specific column by DataFrame.assign() method.
view(steps86)

steps86subset <- subset(steps86,select = c("Year","Ref","age","c1","province","partici3"))
#steps86subset <-
# subset(steps86, select = c("Year","Ref","partici3", "c1", "province", "age"))

#n = count(steps86subset)
Ostan_83 <- read_excel("census_2006/Ostan_83.xlsx")
#Ostan_83 <-
#read_excel("census_2006/Ostan_83.xlsx")

# steps86subset <- cbind(steps86subset, ProvinceName = NA)
for (s in Ostan_83$value)
  
{
  # Replace by Checking Multiple Conditions
  steps86subset$ProvinceName[steps86subset$province == s] <-
    Ostan_83$label[Ostan_83$value == s]
  
}




# Replace by Checking Multiple Conditions
steps86subset$R_C[steps86subset$partici3 == 1] <- "Urban"
steps86subset$R_C[steps86subset$partici3 == 2] <- "Rural"
steps86subset$Sex[steps86subset$c1 == 2] <- "F"
steps86subset$Sex[steps86subset$c1 == 1] <- "M"
AgeGroup <- read_excel("census_2006/AgeGroup.xlsx")
#AgeGroup <- read_excel("census_2006/AgeGroup.xlsx")


for (i in 1:nrow(steps86subset)) {
  # print(i)
  for (F1 in   AgeGroup$From) {
    if (!is.na(steps86subset[i, 'age']) &
        steps86subset[i, 'age'] >= F1 &
        steps86subset[i,'age'] <= AgeGroup$To [AgeGroup$From ==
                                               F1]) {
      steps86subset[i, 'Frm1'] <- F1
      steps86subset[i, 'To1'] <-
        AgeGroup$To [AgeGroup$From == F1]
      
    }
  }
}
library(writexl)
write_xlsx(steps86subset,"../data_raw/2007/steps86subset_F.xlsx")
FileList <- read_excel("census_2006/FileList.xlsx")
AgeGroup <- read_excel("census_2006/AgeGroup.xlsx")



for (name in FileList$FileName) {
  print(name)
  osCode = substring(name, 3, 4)
  osName = Ostan_83$label [Ostan_83$value == as.numeric(as.character(osCode))]
  #ThePath = "census_2006/files/"
  library(stringr)
  
  
  osinf <-
    read_excel(paste("census_2006/files/", name, sep =
                       ""))
  
  # Remove Columns by Range
  osinf <- osinf[, -11:-13]
  
  # Select Rows by list of index values
  osinfSubset <-
    osinf[c(6,
            12,
            18,
            24,
            30,
            36,
            42,
            48,
            54,
            60,
            66,
            72,
            78,
            84,
            90,
            96,
            102,
            108,
            114,
            120,
            126), ] 
  
  
  frm = 0
  to = 4
  FrmTo = data.frame()
  
  for (i in 1:21) {
    output = c(frm, to)
    FrmTo = rbind(FrmTo, output)
    
    
    
    frm = frm + 5
    to = to + 5
  }
  osinfSubset = cbind(osinfSubset, FrmTo)
  colnames(osinfSubset) <-
    c(
      "AgeCat",
      "Sum_M_F",
      "Sum_M",
      "Sum_F",
      "Sum_C_M_F",
      "Sum_C_M",
      "Sum_C_F",
      "Sum_R_M_F",
      "Sum_R_M",
      "Sum_R_F",
      "From",
      "To"
    )
  
  for (se in list("F", "M")) {
    for (ci in list ("Urban", "Rural")) {
      for (ag in AgeGroup$From) {
        steps86subset$cntSmpl[ steps86subset$Sex == se &
                                 steps86subset$R_C == ci &
                                 steps86subset$Frm1 == ag
                               &  steps86subset$ProvinceName == osName] <-
          nrow( steps86subset[ steps86subset$Sex == se &
                                 steps86subset$R_C == ci &
                                 steps86subset$Frm1 == ag &  steps86subset$ProvinceName == osName, ])
      }
    }
  }
  
  #############################################################################
  for (se in list("F", "M")) {
    for (ci in list ("Urban", "Rural")) {
      for (ag in AgeGroup$From) {
        if (se=="F" & ci=="Urban") {
          steps86subset$cnt[ steps86subset$Sex == se &
                               steps86subset$R_C == ci &
                               steps86subset$Frm1 == ag &  steps86subset$ProvinceName==osName] <-
            osinfSubset$Sum_C_F[(osinfSubset$From == ag)]}
        if (se=="F" & ci=="Rural") {
          steps86subset$cnt[ steps86subset$Sex == se &
                               steps86subset$R_C == ci &
                               steps86subset$Frm1 == ag &  steps86subset$ProvinceName==osName] <-
            osinfSubset$Sum_R_F[(osinfSubset$From == ag)]}
        if (se=="M" & ci=="Urban") {
          steps86subset$cnt[ steps86subset$Sex == se &
                               steps86subset$R_C == ci &
                               steps86subset$Frm1 == ag &  steps86subset$ProvinceName==osName] <-
            osinfSubset$Sum_C_M[(osinfSubset$From == ag)]}
        if (se=="M" & ci=="Rural") {
          steps86subset$cnt[ steps86subset$Sex == se &
                               steps86subset$R_C == ci &
                               steps86subset$Frm1 == ag &  steps86subset$ProvinceName==osName] <-
            osinfSubset$Sum_R_M[(osinfSubset$From == ag)]}
      }
    }
  }
  
  
}
##################################################
steps86subset$Weight_83=round(as.numeric( steps86subset$cnt)/as.numeric( steps86subset$cntSmpl),2)

steps86subset$Weight_83[is.na( steps86subset$Weight_83)] = 1


for (i in 1:nrow( steps86subset)) {
  steps86$cnt [steps86$Ref == i] <-
    steps86subset$cnt[ steps86subset$Ref==i]
  steps86$cntSmpl [steps86$Ref == i] <-
    steps86subset$cntSmpl [ steps86subset$Ref==i]
  steps86$Weight_83 [steps86$Ref == i] <-
    steps86subset$Weight_83 [ steps86subset$Ref==i]
}

# library(writexl)
# write_xlsx(steps86,"../data_raw/2007/steps86test.xlsx")
write.csv(steps86, "../data_raw/2007/steps86_weight_standard.csv", row.names = FALSE)

sum(steps86$Weight_83)
