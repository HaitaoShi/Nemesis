#创建Shiny App.
library(shiny)
library(ggplot2)
library(htmlwidgets)
library(rsconnect)
library(tidyverse)
library(tidytext)
library(tidyr)

#读取数据
New_df <- read_csv("9May.csv") 
df_clean <- New_df %>% group_by(Drug_Type_L) %>% summarise(Type_Content=paste(MainContent,collapse = "")) %>% ungroup() 

word_only <- df_clean %>% mutate(Type_Content = str_replace_all(df_clean$Type_Content, "[^[:alpha:]]", " "))

custom_stopwords <- tibble(word=c("mg"), lexicon="custom")

StopWords <- bind_rows(stop_words, custom_stopwords)


AllWordFrequency <- word_only[-11,] %>% unnest_tokens(word,Type_Content) %>% anti_join(StopWords) %>% group_by(Drug_Type_L) %>% count(word,sort = TRUE) %>% mutate(fre=n/sum(n)) %>% mutate(word=reorder(word,n)) %>% slice_head(n=50) %>% ungroup()  


# 创建应用程序界面
ui <- fluidPage(
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


# 创建服务器逻辑
server <- function(input, output) {
  # 过滤数据框
  filtered_df <- reactive({
    AllWordFrequency %>% filter(Drug_Type_L == input$drugtype)
  })
  
  
  # 绘制图表
  output$plot <- renderPlot({
    ggplot(filtered_df(), aes(x = fre, y = reorder_within(word,fre,Drug_Type_L), color = Drug_Type_L)) +
      geom_point(alpha = 0.3, size = 1,colour = "black") +
      geom_text(aes(label = word), nudge_x = 0.1, check_overlap = TRUE, size = 3,colour = "black")+
      scale_y_reordered()+
      scale_x_log10(labels = scales::percent_format()) +
      labs(title = "Word Frequency for All Types of Drugs",
           x = "Word Frequency",
           y = "Words") +
      theme_bw() +
      theme(panel.spacing = unit(0, "lines"),
            strip.background = element_blank(),
            axis.text.x = element_text(size = 10),
            axis.title = element_text(size = 14),
            axis.line = element_line(colour = "black", size = 0.5),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  })
}



# 运行应用程序
setwd("/Users/shihaitao/Documents/DarknetProject/Nemesis/MyApp")
runApp()


#部署我的应用程序

rsconnect::setAccountInfo(name='lcf9k8-haitao0shi',
                          token='32AC6A81D9080BC53C213E6D1E4F7852',
                          secret='PaHFxHfeNXljyGoNIL23gQhchn7gMSjWEKZik+lx')

library(rsconnect)
rsconnect::deployApp()