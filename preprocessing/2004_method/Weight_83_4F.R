library(readxl)
library(haven)
library(tidyverse)
library(dplyr)
Ostan_83 <-
  read_excel("census_2006/Ostan_83.xlsx")

Total83_Subset <- read_excel("../data_raw/2004/Total83_Subset.xlsx")
n = count(Total83_Subset)

FileList <- read_excel("census_2006/FileList.xlsx")
AgeGroup <- read_excel("census_2006/AgeGroup.xlsx")
# FileList1 <- read_excel("census_2006/FileList1.xlsx")
for (name in FileList$FileName) {
  print(name)
  osCode = substring(name, 3, 4)
  osName = Ostan_83$label [Ostan_83$value == as.numeric(as.character(osCode))]
  ThePath = "census_2006/files/"
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
          Total83_Subset$cntSmpl[Total83_Subset$Sex == se &
                                   Total83_Subset$R_C == ci &
                                   Total83_Subset$Frm1 == ag
                                 & Total83_Subset$ProvinceName == osName] <-
            nrow(Total83_Subset[Total83_Subset$Sex == se &
                                  Total83_Subset$R_C == ci &
                                  Total83_Subset$Frm1 == ag & Total83_Subset$ProvinceName == osName, ])
        }
      }
    }
    
    #############################################################################
    for (se in list("F", "M")) {
      for (ci in list ("Urban", "Rural")) {
        for (ag in AgeGroup$From) {
          if (se=="F" & ci=="Urban") {
            Total83_Subset$cnt[Total83_Subset$Sex == se &
                                 Total83_Subset$R_C == ci &
                                 Total83_Subset$Frm1 == ag & Total83_Subset$ProvinceName==osName] <-
              osinfSubset$Sum_C_F[(osinfSubset$From == ag)]}
          if (se=="F" & ci=="Rural") {
            Total83_Subset$cnt[Total83_Subset$Sex == se &
                                 Total83_Subset$R_C == ci &
                                 Total83_Subset$Frm1 == ag & Total83_Subset$ProvinceName==osName] <-
              osinfSubset$Sum_R_F[(osinfSubset$From == ag)]}
          if (se=="M" & ci=="Urban") {
            Total83_Subset$cnt[Total83_Subset$Sex == se &
                                 Total83_Subset$R_C == ci &
                                 Total83_Subset$Frm1 == ag & Total83_Subset$ProvinceName==osName] <-
              osinfSubset$Sum_C_M[(osinfSubset$From == ag)]}
          if (se=="M" & ci=="Rural") {
            Total83_Subset$cnt[Total83_Subset$Sex == se &
                                 Total83_Subset$R_C == ci &
                                 Total83_Subset$Frm1 == ag & Total83_Subset$ProvinceName==osName] <-
              osinfSubset$Sum_R_M[(osinfSubset$From == ag)]}
        }
      }
    }
    
      
}


####################################################
#######################################################################################
uu=subset(Total83_Subset, Total83_Subset$ProvinceName == 'Yazd')
uu$weight <- as.numeric(uu$cnt)/uu$cntSmpl
# uu <- cbind(uu,uu$cnt/uu$cntSmp )










#######################################################################################
uu=subset(Total83_Subset, Total83_Subset$ProvinceName == 'Yazd')
uu$weight <- as.numeric(uu$cnt)/uu$cntSmpl
# uu <- cbind(uu,uu$cnt/uu$cntSmp )
