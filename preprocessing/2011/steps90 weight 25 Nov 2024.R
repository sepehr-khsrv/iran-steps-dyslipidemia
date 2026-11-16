library(readxl)
library(haven)
library(tidyverse)
library(dplyr)
library(writexl)
Steps90 <-read_dta("../data_raw/2011/Steps90 (1).dta")
Os90 <- read_excel("census_2011/Os90.xlsx")
##################################################################
#create data frame with 2 rows and nrow columns
df <- data.frame(matrix(ncol = 2, nrow = nrow((Steps90))))

#provide column names
colnames(df) <- c('Year', 'ID')
df$Year="90"
df$ID <- 1:nrow(df) 
Steps90 <- cbind(df,Steps90)
################################
data_unique <- unique(Steps90[ , c("province_census", "district")])  
Os90 <- data.frame()
for (i in 1:nrow(data_unique)) {
  a=substr(data_unique[i,2],1,1)
  print(a)
  if (is.element(a, LETTERS)){
    
    Os90 <- rbind(Os90,data_unique[i,])
  }
}

for (prov in Os90$province_census) {
  print(prov)
  
  Steps90$district [Steps90$province_census==prov] <- Os90$district[(Os90$province_census==prov)]
  
}


# Create New DataFrame of Specific column by DataFrame.assign() method.

Total90_Subset <-
  subset(Steps90, select = c("Year","ID","i1d2", "c1", "province_census", "district", "age2"))

n = count(Total90_Subset)


# Replace by Checking Multiple Conditions
Total90_Subset$R_C[Total90_Subset$i1d2 == 1] <- "Urban"
Total90_Subset$R_C[Total90_Subset$i1d2 == 2] <- "Rural"
Total90_Subset$Sex[Total90_Subset$c1 == 0] <- "F"
Total90_Subset$Sex[Total90_Subset$c1 == 1] <- "M"

# applying weight 


Steps90_F <- Steps90
FileList <- read_excel("census_2011/FileList.xlsx")


for (FileName in FileList$FileName) {
  #FileName="Os11.xls"
  print(FileName)
  osCode = substring(FileName, 3, 4)
  osFileName = Os90$district [Os90$province_census == as.numeric(as.character(osCode))]
  library(stringr)
  osinf <-
    read_excel(paste("census_2011/90/", FileName, sep =
                       ""))
  
  # Remove Columns by Range
  osinf <- osinf[, -5]
  osinf <- osinf[,-11:-13]
  osinfSubset = data.frame()
  # Select Rows by list of index values
  grp = 10 
  for (i in seq(6, 124)) {
    if(i!=grp){ 
      osinfSubset = rbind(osinfSubset,osinf[i,])
      
    }
    
    
    else{ 
      grp = grp + 6
      
    }
  }
  
  
  colnames(osinfSubset) <-
    c(
      "Age2",
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
      for (ag in osinfSubset$Age2) {
        Total90_Subset$cntSmpl[Total90_Subset$Sex == se &
                                 Total90_Subset$R_C == ci &
                                 Total90_Subset$age2 == str_trim(substr(ag,1,2),side = c("both","left","right")) 
                               &
                                 Total90_Subset$district == osFileName] <-
          nrow(Total90_Subset[Total90_Subset$Sex == se &
                                Total90_Subset$R_C == ci &
                                Total90_Subset$age2 == str_trim(substr(ag,1,2),side = c("both","left","right")) &
                                Total90_Subset$district == osFileName
                              & ! (is.na(Total90_Subset$age2)),])
      }
    }
  }
  
  #############################################################################
  for (se in list("F", "M")) {
    for (ci in list ("Urban", "Rural")) {
      for (ag in osinfSubset$Age2) {
        if (se == "F" & ci == "Urban") {
          Total90_Subset$cnt[Total90_Subset$Sex == se &
                               Total90_Subset$R_C == ci &
                               Total90_Subset$age2 == str_trim(substr(ag,1,2),side = c("both","left","right")) &
                               Total90_Subset$district == osFileName] <-
            osinfSubset$Sum_C_F[(osinfSubset$Age2 == ag)]
        }
        if (se == "F" & ci == "Rural") {
          Total90_Subset$cnt[Total90_Subset$Sex == se &
                               Total90_Subset$R_C == ci &
                               Total90_Subset$age2 == str_trim(substr(ag,1,2),side = c("both","left","right")) &
                               Total90_Subset$district == osFileName] <-
            osinfSubset$Sum_R_F[(osinfSubset$Age2 == ag)]
        }
        if (se == "M" & ci == "Urban") {
          Total90_Subset$cnt[Total90_Subset$Sex == se &
                               Total90_Subset$R_C == ci &
                               Total90_Subset$age2 == str_trim(substr(ag,1,2),side = c("both","left","right")) &
                               Total90_Subset$district == osFileName] <-
            osinfSubset$Sum_C_M[(osinfSubset$Age2 == ag)]
        }
        if (se == "M" & ci == "Rural") {
          Total90_Subset$cnt[Total90_Subset$Sex == se &
                               Total90_Subset$R_C == ci &
                               Total90_Subset$age2 == str_trim(substr(ag,1,2),side = c("both","left","right")) &
                               Total90_Subset$district == osFileName] <-
            osinfSubset$Sum_R_M[(osinfSubset$Age2 == ag)]
        }
      }
    }
  }
  
  
}
##################################################
Total90_Subset$Weight_90 = round(as.numeric(Total90_Subset$cnt) / as.numeric(Total90_Subset$cntSmpl),
                                 2)

Total90_Subset$Weight_90[is.na(Total90_Subset$Weight_90)] = 1


for (i in Total90_Subset$ID) {
  Steps90_F$cnt [Steps90_F$ID == i] <-
    Total90_Subset$cnt[Total90_Subset$ID == i]
  Steps90_F$cntSmpl [Steps90_F$ID == i] <-
    Total90_Subset$cntSmpl [Total90_Subset$ID == i]
  Steps90_F$Weight_90 [Steps90_F$ID == i] <-
    Total90_Subset$Weight_90 [Total90_Subset$ID == i]
}
view(Steps90_F$Weight_90)
sum(Steps90_F$Weight_90)
# aaaa <-  subset(Steps90_F, select = c("Year","ID","cnt", "cntSmpl", "Weight_90"))

# write_xlsx(Steps90_F,"../data_raw/2011/Steps90_2F.xlsx")
# Replace 'Steps90_F' with your actual dataframe variable

write.csv(Steps90_F, "../data_raw/2011/Steps90_weighted.csv", row.names = FALSE)


