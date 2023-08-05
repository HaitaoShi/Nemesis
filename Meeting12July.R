library(tidyverse)
library(forcats)
library(stringr)
library(tidytext)

# load data
data  <-  read_csv("/Users/shihaitao/Documents/DarknetProject/Nemesis/complete.csv")

data_meeting <- data %>% select(File_Name,Vendor_Name,Title,Rating,Reviews,Sales,Ship_From,Ship_To,Availablity,Ship_Methods)

write_csv(data_meeting, "12Julymeeting.csv")

data_meeting %>% select(Vendor_Name,Rating,Reviews,Sales) %>% unique() %>% arrange(Vendor_Name)
