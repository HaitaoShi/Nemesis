library(tidyverse)
setwd("/Users/shihaitao/Documents/DarknetProject/Nemesis/DataOriginal")
file_list <- list.files(pattern = ".txt")

DF <- data.frame()

for (file in file_list) {
  content <- readLines(file)
  line_number <- grep("Rating", content) -1
  extractedInfo <- data.frame(vendername=content[line_number])
  DF <- bind_rows(DF, extractedInfo)
}
File_Name <- file_list %>% str_extract("\\d+")

DF <- DF %>% mutate(File_Name=File_Name,.before = vendername) %>% as_tibble()


New_df <- read_csv("9May.csv") 

#updated dataframe with vendor names

New_df <- left_join(New_df, DF, by = "File_Name")


# split the strings from Refund_Review to Refund and Review
New_df <- New_df %>% rename(Vender_Name=vendername) %>% separate(Refund_Review, into = c("Refund", "Review"), sep = "__ Sort comments by Top New", remove = TRUE)

write_csv(New_df, "/Users/shihaitao/Documents/DarknetProject/Nemesis/complete.csv")


