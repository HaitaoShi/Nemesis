library(tidyverse)


refund_Analysis <- read_csv("/Users/shihaitao/Documents/DarknetProject/Nemesis/complete.csv")

refund_Analysis <- refund_Analysis %>% select(File_Name, Vender_Name,Drug_Type, Rating,Sales,Refund) 

#remove the rows with NA values in the column of Refund
refund_Analysis <- refund_Analysis %>% filter(!is.na(Refund))

# see the row number of the data frame
nrow(refund_Analysis)

# If the Drug_Type and vender_Name are the same, then we compare the content in Refund. 
# If the Refund is the same, then we keep the first row and remove the rest of the rows.
# If the Refund is different, then we keep all the rows.
refund_Analysis<- refund_Analysis %>% group_by(Drug_Type,Vender_Name) %>% 
  filter(n() == 1 | n() > 1 & !duplicated(Refund)) %>% 
  ungroup()

# see the row number of the data frame
nrow(refund_Analysis)

view(refund_Analysis)

# remove row 10 with "Refund olicy N/A" values
refund_Analysis <- refund_Analysis[-10,]

# rearrange the data frame with the Sales number decreasingly
refund_Analysis <- refund_Analysis %>% arrange(desc(Sales))
#-----------------------------------------------------------------
# test whether the length of words in the Refund column is related to the number in the Sales column
refund_Analysis$Refund_Length <- sapply(strsplit(as.character(refund_Analysis$Refund), " "), length)
cor(refund_Analysis$Refund_Length, refund_Analysis$Sales)
#visualise the relationship between the length of words in the Refund column and the number in the Sales column
ggplot(refund_Analysis, aes(x = Refund_Length, y = Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
model <- lm(Sales ~ Refund_Length, data = refund_Analysis)
summary(model)
#Results:  a weak positive correlation between the length of words in the Refund column and the number in the Sales column. 
#-----------------------------------------------------------------

