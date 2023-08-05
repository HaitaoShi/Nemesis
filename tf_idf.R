# Please refer to Zipf's Law for more information. 

library(tidyverse)
library(forcats)
library(stringr)
library(tidytext)

# load data
data  <-  read_csv("/Users/shihaitao/Documents/DarknetProject/Darknet/complete.csv")

summary(data)
# Select the columns "Drug_Type_L" and "MainContent" from the data frame "data"
# Group the data by "Drug_Type_L" and concatenate the "MainContent" strings for each group into a single string
# Remove the 6th and 11th rows from the resulting data frame (NA and Other)
# Split the concatenated strings into individual words and remove all digits and units (mg, g, ml, l)
# Count the frequency of each word for each group, and sort the results in descending order
word_only <- data %>% 
  select("Drug_Type_L", "MainContent") %>% 
  group_by(Drug_Type_L) %>% 
  summarise(Type_Content=paste(MainContent,collapse = "")) %>% 
  slice(c(-6,-11)) %>% 
  unnest_tokens(word, Type_Content) %>%  
  mutate(word = str_remove_all(word, "\\d+")) %>% 
  mutate(word = str_remove_all(word, "mg|g|ml|l")) %>% 
  filter(word != "") %>% 
  count(Drug_Type_L, word, sort = TRUE) %>% 
  ungroup()

# Calculate the TF-IDF score for each word in each group
word_tf_idf <- word_only %>% 
  bind_tf_idf(word, Drug_Type_L, n)

# Sort the results by TF-IDF score in descending order and select the top 15 words for each group
word_top15 <- word_tf_idf %>% 
  group_by(Drug_Type_L) %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(15,tf_idf) %>% 
  ungroup()

# Plot the results
word_top15 %>% ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Drug_Type_L)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~Drug_Type_L, ncol = 3, scales = "free") + 
  labs(x = "tf-idf", y = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(margin = margin(r = 5), lineheight = 0.8))

ggsave("/Users/shihaitao/Documents/CryptomarketProject/Cryptomarket/Images/tf_idf.png", plot = last_plot(), dpi = 300, width = 8, height = 10)
