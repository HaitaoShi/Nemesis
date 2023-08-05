

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

# 创建服务器逻辑
function(input, output) {
  # 过滤数据框
  filtered_df <- reactive({
    AllWordFrequency %>% filter(Drug_Type_L == input$drugtype)
  })
  
  # 设置标题
  output$title <- renderText({
    paste("Word Frequency for", input$drugtype)
  })
  
  # 绘制图表
  output$plot <- renderPlotly({
    ggplot(filtered_df(), aes(x = n, y = reorder_within(word, n, Drug_Type_L))) +
      geom_col() +
      scale_y_reordered() +
      labs(title = paste("Word Frequency for", input$drugtype),
           x = "Word Frequency",
           y = "Words") +
      theme_bw() +
      theme(panel.spacing = unit(0, "lines"),
            strip.background = element_blank(),
            axis.text= element_text(size = 6),
            axis.title = element_text(size = 14),
            axis.line = element_line(colour = "black", linewidth = 0.5)) -> plot_obj
    
    # 使用ggplotly将ggplot2对象转换为plotly对象
    ggplotly(plot_obj) %>% 
      # 配置鼠标悬停时显示的信息
      style(hoverinfo = "text",
            text = filtered_df()$n)
  })
}