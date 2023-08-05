library(tidytext)
library(tidyverse)
library(scales)

df <- read_csv("8May.csv") 
Description <- df$Main_content %>% paste(collapse = "") %>% str_split(pattern = "\n")  

Description <- Description[[1]] 
text_df <- tibble(line=1:length(Description),text=Description) %>% filter(str_length(str_replace_all(text, "\\s", "")) > 0) #remove "  ", "", etc.

text_df <- mutate(text_df,line=1:length(text_df$text))
text_df %>% unnest_tokens(word,text)


##Too many kinds of drugs, so I created the types of drugs at a broader level.

##df_clean <- df %>% group_by(Drug_Type) %>% summarise(Type_Content=paste(df$Main_content,collapse = "")) %>% ungroup() 
  
New_df <- df %>% mutate(Drug_Type_L=str_replace(df$Drug_Type, "».*",""))   

##remove white space

New_df <- New_df %>% mutate(Drug_Type_L=str_trim(Drug_Type_L)) 

## Split Main_content into MainContent and Refund_Review
 
New_df <- New_df %>% mutate(MainContent = str_extract(Main_content, "(?s).*?(?=Refund policy)"),
                            Refund_Review = str_extract(Main_content, "(?s)Refund policy.*"))

##save the new data frame
write_csv(New_df, "9May.csv")

##See the differences between different drug types at a broader level.

df_clean <- New_df %>% group_by(Drug_Type_L) %>% summarise(Type_Content=paste(MainContent,collapse = "")) %>% ungroup() 


## remove the symbols and numbers in df_clean

word_only <- df_clean %>% mutate(Type_Content = str_replace_all(df_clean$Type_Content, "[^[:alpha:]]", " "))

##I want to see how ecstasy was described 

test_Ecstasy <- word_only[4,] %>% unnest_tokens(word,Type_Content)

 
##Add the words that I don't want them to show. We can always add more in word = c ()
custom_stopwords <- tibble(word=c("mg"), lexicon="custom")

StopWords <- bind_rows(stop_words, custom_stopwords)


FreEcstasy <- test_Ecstasy %>% anti_join(StopWords) %>% 
  count(word,sort = TRUE) %>% mutate(word=reorder(word,n)) %>% slice_head(n=50) 

FreEcstasy %>% ggplot(aes(n,word)) + geom_col() + labs(y=NULL) + ggtitle("The Most Common Words in Ecstasy Description (top50)")

ggsave("Images/ecstasy_wordcloud.png", plot = last_plot(), dpi = 300, width = 8, height = 6)


#I am intrested in compare the word frequency between Cannabis and Ecstasy (most common 50)
Canabis_freq <- word_only[2,] %>% unnest_tokens(word,Type_Content) %>% anti_join(StopWords) %>% 
  count(word,sort = TRUE)   %>% mutate(DrugName="Canabis",fre=n/sum(n)) %>%mutate(word=reorder(word,n)) %>% slice_head(n=50)

Ecstasy_freq <- word_only[4,] %>% unnest_tokens(word,Type_Content) %>% anti_join(StopWords) %>% 
  count(word,sort = TRUE) %>%  mutate(DrugName="Ecstasy",fre=n/sum(n)) %>%mutate(word=reorder(word,n)) %>% slice_head(n=50)

frequency <- bind_rows(Canabis_freq,Ecstasy_freq) 
 
 
ggplot(frequency, aes(x = fre, y = reorder_within(word,fre,DrugName), color = DrugName)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_text(aes(label = word), nudge_x = 0.1, check_overlap = TRUE, size = 2.5)+
  scale_y_reordered()+
  scale_x_log10(labels = percent_format()) +
  theme(plot.title = element_text(size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.line = element_line(colour = "black", size = 0.5)) +
  labs(title = "Word Frequency for Canabis and Ecstasy",
       x = "Word Frequency",
       y = "Words") +
  theme(plot.margin = margin(1, 1, 0.5, 1, "cm"),
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~DrugName, scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(panel.spacing = unit(0, "lines"), strip.background = element_blank())

ggsave("Images/CanabisVSEcstasy.png", plot = last_plot(), dpi = 300, width = 8, height = 6)
 

### Another Way to do so
library(ggplot2)
library(patchwork)

p1 <- ggplot(Canabis_freq, aes(x = fre, y = word)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_text(aes(label = word), nudge_x = 0.1, check_overlap = TRUE, size = 2.5)+
  scale_x_log10(labels = percent_format()) +
  scale_color_discrete(guide = FALSE) +
  theme(plot.title = element_text(size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.line = element_line(colour = "black", size = 0.5)) +
  labs(title = "Canabis",
       x = "",
       y = "") +
  theme(plot.margin = margin(1, 1, 0.5, 1, "cm"),
        plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  theme(panel.spacing = unit(0, "lines"), strip.background = element_blank()) 



p2 <- ggplot(Ecstasy_freq, aes(x = fre, y = word)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_text(aes(label = word), nudge_x = 0.1, check_overlap = TRUE, size = 2.5)+
  scale_x_log10(labels = percent_format()) +
  scale_color_discrete(guide = FALSE) +
  theme(plot.title = element_text(size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.line = element_line(colour = "black", size = 0.5)) +
  labs(title = "Ecstasy",
       x = "",
       y = "") +
  theme(plot.margin = margin(1, 1, 0.5, 1, "cm"),
        plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  theme(panel.spacing = unit(0, "lines"), strip.background = element_blank()) 


wrap_plots(p1,p2)
ggsave("Images/CanabisVSEcstasy2.png", plot = last_plot(), dpi = 300, width = 8, height = 6)


## WordFrequencyForAll


AllWordFrequency <- word_only[-11,] %>% unnest_tokens(word,Type_Content) %>% anti_join(StopWords) %>% group_by(Drug_Type_L) %>% count(word,sort = TRUE) %>% mutate(fre=n/sum(n)) %>% mutate(word=reorder(word,n)) %>% slice_head(n=50) %>% ungroup()  


ggplot(data = AllWordFrequency, aes(x = fre, y = reorder_within(word, fre, Drug_Type_L), color = Drug_Type_L)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_text(aes(label = word), nudge_x = 0.1, check_overlap = TRUE, size = 2)+
  scale_y_reordered() +
  scale_x_log10(labels = percent_format()) +
  labs(title = "Word Frequency for All Types of Drugs",
       x = "Word Frequency",
       y = "Words") +
  theme_bw() +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


ggsave("/Users/shihaitao/Documents/DarknetProject/Darknet/Images/WordFrequencyForAll.png", plot = last_plot(), dpi = 300, width = 8, height = 6)

# Not enough space, so a Shiny App would be a better choice. 


