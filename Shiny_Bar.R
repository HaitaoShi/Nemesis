library(shiny)
library(ggplot2)
library(htmlwidgets)
library(rsconnect)
library(tidyverse)
library(tidytext)
library(tidyr)
library(plotly)

# Load Data
New_df <- read_csv("9May.csv") 
df_clean <- New_df %>% 
  group_by(Drug_Type_L) %>% 
  summarise(Type_Content=paste(MainContent,collapse = "")) %>% 
  ungroup()

word_only <- df_clean %>% 
  mutate(Type_Content = str_replace_all(df_clean$Type_Content, "[^[:alpha:]]", " "))

custom_stopwords <- tibble(word=c("mg"), lexicon="custom")
StopWords <- bind_rows(stop_words, custom_stopwords)

AllWordFrequency <- word_only[-11,] %>% 
  unnest_tokens(word,Type_Content) %>% 
  anti_join(StopWords) %>% 
  group_by(Drug_Type_L) %>% 
  count(word,sort = TRUE) %>% 
  mutate(fre=n/sum(n)) %>% 
  mutate(word=reorder(word,n)) %>% 
  slice_head(n=50) %>% 
  ungroup()

#write_csv(AllWordFrequency, "AllWordFrequency.csv")
#AllWordFrequency <- read_csv("AllWordFrequency.csv") 



# Creating the application interface
ui <- fluidPage(
  titlePanel(uiOutput("title")),
  sidebarLayout(
    sidebarPanel(
      h3("Drug Types"),
      selectInput("drugtype", "Select drug type:", choices = unique(AllWordFrequency$Drug_Type_L), selected = "Benzodiazepine (Benzos)")
    ),
    mainPanel(
      fluidRow(
        plotlyOutput("plot")
      )
    )
  )
)

# Creating server logic
server <- function(input, output) {
  # Filter Data Boxes
  filtered_df <- reactive({
    AllWordFrequency %>% filter(Drug_Type_L == input$drugtype)
  })
  
  # Add title
  output$title <- renderText({
    paste("Word Frequency for", input$drugtype)
  })
  
  # Plot
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
    
    # Converting ggplot2 objects to plotly objects using ggplotly
    ggplotly(plot_obj) %>% 
      # Configure the message displayed on mouse hover
      style(hoverinfo = "text",
            text = filtered_df()$n)
  })
}

shinyApp(ui, server)




## Deploy the App.
 

library(shiny)
library(ggplot2)
library(htmlwidgets)
library(rsconnect)
library(tidyverse)
library(tidytext)
library(tidyr)
library(plotly)
setwd("/Users/shihaitao/Documents/DarknetProject/Nemesis/MyApp_bar")
runApp()


rsconnect::setAccountInfo(name='lcf9k8-haitao0shi',
                          token='32AC6A81D9080BC53C213E6D1E4F7852',
                          secret='PaHFxHfeNXljyGoNIL23gQhchn7gMSjWEKZik+lx')

library(rsconnect)
rsconnect::deployApp()
