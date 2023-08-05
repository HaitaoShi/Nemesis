library(tidyverse)
setwd("/Users/shihaitao/Documents/DarknetProject/Nemesis/DataOriginal")
file_list <- list.files(pattern = ".txt")
for (file in file_list) {
  content <- readLines(file)
  content <- paste(content, collapse = "\n")
  content <- str_remove(content, "(?s)\\* Market.*accepted here")  
  content <- str_remove_all(content,"!.*[jpg,png]\\)")  
  content <- str_remove_all(content,"\\* \n")
  content <- str_remove(content, "(?s)## You may be interested.*")
  content <- str_remove(content, "(?s)1 2 3 4 5.*on our forum\\.")
  content <- str_replace_all(content,"(\n)+", "\n")
  content <- str_remove(content, "(?s)URL:\\s.*?(?=TITLE:)")
  content <- strsplit(content, "\n")[[1]]  
  writeLines(content, file)
}

## I remove any file that do not have 'drugs' in it. 
dir_path <- "/Users/shihaitao/Documents/DarknetProject/Nemesis/DataCleanedWithDrugsOnly"

keep_files <- sapply(file_list, function(file){
  file_contents <- readLines(file.path(dir_path,file))
  EachLine <- str_detect(file_contents,fixed("drugs",ignore_case = TRUE))
  sum(EachLine)
})

files_to_remove <- keep_files[keep_files==0]
files_to_remove <- names(files_to_remove)
file.remove(file.path(dir_path, files_to_remove))

## Now I am going to get the content that I want to run for analysis

library(tidyverse)

# Set up a list of file paths
dir_path <- "/Users/shihaitao/Documents/DarknetProject/Nemesis/DataCleanedFurther1"

file_list <- list.files(dir_path, pattern = ".txt")

File_Name <- file_list %>% str_extract("\\d+")


# Create an empty data frame to store the extracted information
df <- data.frame()

# Loop over the files and extract the information
for (i in 1:length(file_list)) {
  file <- readLines(file_list[i]) %>% paste(collapse = "\n")
  
  # Use regular expressions to extract the desired information
  # and store it in a data frame
  TITLE <- file %>%
    str_extract("(?<=TITLE:).*?(?= \\|)") 
  
  Drug_Type <- file %>%
    str_extract("(?<=Drugs  » ).*?(?=\\n)")
  
  Rating <- file %>%
    str_extract("(?<=Rating:).*?(?=\\n)")
  
  Reviews <- file %>% 
    str_extract("(?<=Reviews:).*?(?=\\n)")
  
  Sales<- file %>% 
    str_extract("(?<=Sales:).*?(?=\\n)")
  
  Ship_From <- file %>% 
    str_extract("(?<=Shipping from\\s).*?(?=\\sto)")
  
  Ship_To <- file %>% 
    str_extract("(?<=to\\s).*?(?=\\nShipping methods)")
  
  Ship_Methods <- file %>% 
    str_extract("(?s)(?<=Shipping methods:).*?(?=\\nPosted)")
  
  Main_content <- file %>% 
    str_extract("(?s)(?<=\\* Reviews).*")
  
  # Combine the extracted information into a data frame
  extracted_info <- data.frame(
    Title=TITLE, 
    Drug_Type=Drug_Type,
    Rating=Rating,
    Reviews=Reviews,
    Sales=Sales,
    Ship_From=Ship_From,
    Ship_To=Ship_To,
    Ship_Methods=Ship_Methods,
    Main_content=Main_content)
  
  
  # Append the extracted information to the data frame
  df <- bind_rows(df, extracted_info)
}


df <- df %>% mutate(File_Name=File_Name,.before = Title)



#Delete those that are not drug related
which(is.na(df$Drug_Type))

## Based on the titles we know that they are not selling drugs. 
df <- df[-c(35,37,69,84,98, 113, 124, 135, 360, 485),]
 
#FurtherClean:

##1. Delete: Vendor on vacation......
df <- df %>% mutate(Ship_Methods = str_replace(Ship_Methods,"(?s)Vendor on vacation.*", ""))

## add availability based on the string "Not available"
df <- df %>% mutate(Availablity= if_else(str_detect(Ship_Methods,"Not available"),"No","Yes"))
 
## Delete: Not available in Ship_Methods
df <- df %>% mutate(Ship_Methods = str_replace(Ship_Methods,"(?s)Not available.*", ""))
df <- as.tibble(df)
#8 May Save the data
setwd("/Users/shihaitao/Documents/DarknetProject/Nemesis")
write_csv(df, "8May.csv")
#mydata <- read_csv("8May.csv") 
##I tried to export it in excel format, but the String exceeds Excel's limit.
##install.packages("writexl")
##library(writexl)
##write_xlsx(df, "8May.xlsx")


