library(readxl)
library(haven)
library(tidyverse)
library(dplyr)
library(writexl)
Total83_new <-
  read_dta("../data_raw/2004/Total83.dta")
##################################################################
#create data frame with 2 rows and nrow columns
df <- data.frame(matrix(ncol = 2, nrow = nrow((Total83_new))))

#provide column names
colnames(df) <- c('Year', 'Ref')
df$Year="83"
df$Ref <- 1:nrow(df) 
Total83_new <- cbind(df,Total83_new)
write_xlsx(Total83_new,"../data_raw/2004/Total83_new_F.xlsx")
##################################################################

# Create New DataFrame of Specific column by DataFrame.assign() method.

Total83_Subset <-
  subset(Total83_new, select = c("Year","Ref","i1d2", "c1", "province_census", "age"))

n = count(Total83_Subset)
Ostan_83 <-
  read_excel("census_2006/Ostan_83.xlsx")

# Total83_Subset <- cbind(Total83_Subset, ProvinceName = NA)
for (s in Ostan_83$value)
  
{
  # Replace by Checking Multiple Conditions
  Total83_Subset$ProvinceName[Total83_Subset$province_census == s] <-
    Ostan_83$label[Ostan_83$value == s]
  
}

# Replace by Checking Multiple Conditions
Total83_Subset$R_C[Total83_Subset$i1d2 == 1] <- "Urban"
Total83_Subset$R_C[Total83_Subset$i1d2 == 2] <- "Rural"
Total83_Subset$Sex[Total83_Subset$c1 == 0] <- "F"
Total83_Subset$Sex[Total83_Subset$c1 == 1] <- "M"
AgeGroup <- read_excel("census_2006/AgeGroup.xlsx")


for (i in 1:nrow(Total83_Subset)) {
  # print(i)
  for (F1 in   AgeGroup$From) {
    if (!is.na(Total83_Subset[i, 'age']) &
        Total83_Subset[i, 'age'] >= F1 &
        Total83_Subset[i,'age'] <= AgeGroup$To [AgeGroup$From ==
                                                             F1]) {
      Total83_Subset[i, 'Frm1'] <- F1
      Total83_Subset[i, 'To1'] <-
        AgeGroup$To [AgeGroup$From == F1]
      
    }
  }
}
#library(writexl)
# write_xlsx(Total83_Subset,"../data_raw/2004/Total83_Subset_F.xlsx")
