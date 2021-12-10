library(httr)
library(jsonlite)
library(shiny)
library(tidyverse)
library(dplyr)
library(shinycssloaders)
library(thematic)
library(curl)
library(plyr)
library(magrittr)
library(lubridate)

# Get the data from an API

url <- "https://covid-api.mmediagroup.fr"
resp <- GET(url, path = "v1/history?country=All&status=confirmed")

list_data_structure <- jsonlite::fromJSON(
  rawToChar(resp$content)
)

ds <- lapply(list_data_structure, function(x) {
  x[sapply(x, is.null)] <- 0
  unlist(x)
})

complete_df <- plyr::ldply(ds, rbind)
complete_df <- complete_df[!(complete_df$.id %in% c('Global')), ]

dates_only_df <- complete_df[startsWith(names(complete_df), "All.dates")]
general_df <-complete_df[c(2,6,10)]
stat_df <- complete_df[c(3:5)]

convert_factors_to_numeric <- function(my_dataframe) { 
  as.data.frame(lapply(my_dataframe,
                       function(x) {
                         if (is.factor(x)) {
                           as.numeric(as.character(trimws(x),
                                                   which = "both"))
                         } else{
                           as.numeric(x)
                         }
                       }
  ),
  stringsAsFactors = FALSE)
}

dates_converted <- convert_factors_to_numeric(dates_only_df) 
stat_converted <- convert_factors_to_numeric(stat_df)

df <- cbind(general_df,stat_converted)
df <- cbind(df, dates_converted)

colnames(df) <- sub("All.", "", colnames(df))
colnames(df) <- sub("dates.", "", colnames(df))

df <- df %>%
  add_column(new_cases = df[[7]]-df[[8]],
             .after = 6) 

# Shiny App

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel(
    "Covid-19 App"
  ),
  navlistPanel(
    tabPanel("Start", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("cases_select", 
                             "Specify the output to be shown on the plot:", 
                             c("New cases", "Recovered", "Deaths" )),
                 actionButton("cases_btn", "Show", class = "btn-warning")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Barchart",
                            withSpinner(plotOutput("plot"))
                   ),
                   tabPanel("NA for the following countries",
                            verbatimTextOutput("nulls"))
                 )
               )
             )
    ),
    tabPanel("Historical data", "Animated historical data for each continent"),
    tabPanel("Country comparison", 
             fluidRow(
               column(6,
                      selectInput("continent1", 
                                  "Select continent:", 
                                  choices = unique(df$continent)),
                      selectInput("country1", 
                                  "Select first country to compare:", 
                                  choices = NULL),
                      tableOutput("data1")
               ),
               column(6,
                      selectInput("continent2", 
                                  "Select continent:", 
                                  choices = unique(df$continent)),
                      selectInput("country2", 
                                  "Select second country to compare:", 
                                  choices = NULL),
                      tableOutput("data2")
               )
             ),
             fluidRow(
               withSpinner(plotOutput("plot_comparison"))
             )
    )
  )
)

server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  observeEvent(input$cases_select, {
    label <- paste0("Show ", input$cases_select)
    updateActionButton(inputId = "cases_btn", label = label)
  })
  
  output$plot <- renderPlot({
    plot(df %>% 
           arrange(desc(new_cases))%>%
           head(10)%>%
           ggplot(aes(x = reorder(country, new_cases),
                      y = new_cases)) +
           geom_bar(stat = "identity", color = "black", fill = "darkturquoise") + 
           coord_flip() +
           xlab ("Country") +
           ylab ("New Covid-19 cases"))
  })
  
  output$nulls <- renderPrint({
    for (i in 1:length(df$new_cases)){
      if (is.na(df$new_cases[i]) == TRUE) {
        print(df$country[i])
      }
    }
  })
  
  continent1 <- reactive({
    filter(df, continent == input$continent1)
  })
  
  observeEvent(continent1(), {
    choices <- unique(continent1()$country)
    updateSelectInput(inputId = "country1", choices = choices) 
  })
  
  continent2 <- reactive({
    filter(df, continent == input$continent2)
  })
  
  observeEvent(continent2(), {
    choices <- unique(continent2()$country)
    updateSelectInput(inputId = "country2", choices = choices) 
  })
  
  output$data1 <- renderTable({
    req(input$country1)
    continent1() %>% 
      filter(country == input$country1) %>% 
      select(population, new_cases)
  })  
  
  output$data2 <- renderTable({
    req(input$country2)
    continent2() %>% 
      filter(country == input$country2) %>% 
      select(population, new_cases)
  })
  
  # space for plot #
}

shinyApp(ui, server)
