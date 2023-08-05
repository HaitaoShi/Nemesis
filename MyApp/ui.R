

library(shiny)
library(ggplot2)
library(htmlwidgets)
library(rsconnect)
library(tidyverse)
library(tidytext)
#读取数据
New_df <- read_csv("9May.csv") 
df_clean <- New_df %>% group_by(Drug_Type_L) %>% summarise(Type_Content=paste(MainContent,collapse = "")) %>% ungroup() 

word_only <- df_clean %>% mutate(Type_Content = str_replace_all(df_clean$Type_Content, "[^[:alpha:]]", " "))

custom_stopwords <- tibble(word=c("mg"), lexicon="custom")

StopWords <- bind_rows(stop_words, custom_stopwords)


AllWordFrequency <- word_only[-11,] %>% unnest_tokens(word,Type_Content) %>% anti_join(StopWords) %>% group_by(Drug_Type_L) %>% count(word,sort = TRUE) %>% mutate(fre=n/sum(n)) %>% mutate(word=reorder(word,n)) %>% slice_head(n=50) %>% ungroup()  
# 创建应用程序界面
fluidPage(
  titlePanel("Word Frequency for All Types of Drugs"),
  sidebarLayout(
    sidebarPanel(
      h3("Drug Types"),
      selectInput("drugtype", "Select drug type:", choices = unique(AllWordFrequency$Drug_Type_L), selected = "Benzodiazepine (Benzos)")
    ),
    mainPanel(
      fluidRow(
        plotOutput("plot")
      )
    )
  )
)
