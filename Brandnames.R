library(tidyverse)
library(writexl)



# Read in the data from the csv file
DF <- read_csv("brand_name.csv")

# brand_name <- tibble(FileName = DF$File_Name, Title = DF$Title, BrandName = "", Drug_Type=DF$Drug_Type)
str(DF)

#save DF as xlsx file
write_xlsx(DF, "brand_name.xlsx")


 
