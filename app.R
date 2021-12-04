library(httr)
library(jsonlite)
library(shiny)
library(tidyverse)
library(dplyr)
library(shinycssloaders)
library(thematic)

# Getting data from an API

# statistics data set

stat_url <- "https://covid-193.p.rapidapi.com/statistics"

res_stat <- GET(stat_url, 
                add_headers(
                  `x-rapidapi-host` = 'covid-193.p.rapidapi.com', 
                  `x-rapidapi-key` = Sys.getenv("SHINYAPP_APIKEY")
                )
)

stat_data <- jsonlite::fromJSON(rawToChar(res_stat$content))
stat_df <- as.data.frame(stat_data$response)

# historical data set

hist_url <- "https://covid-193.p.rapidapi.com/history"

queryString <- list(
  country = "usa", 
  day = "2020-06-02"
)

res_hist <- GET(hist_url, 
                add_headers(
                  `x-rapidapi-host` = 'covid-193.p.rapidapi.com', 
                  `x-rapidapi-key` = Sys.getenv("SHINYAPP_APIKEY")), 
                  query = queryString
)


hist_data <- jsonlite::fromJSON(rawToChar(res_hist$content))
hist_df <- as.data.frame(hist_data$response)

# Data manipulation

df <- stat_df[ !is.na(stat_df$population) == TRUE, ]
#hist_df <- hist_df[ !is.na(hist_df$population) == TRUE, ]

df$new_cases <- as.numeric(df$cases$new)
df$deaths <- as.numeric(df$deaths$new)
df$recovered <- as.numeric(df$cases$recovered)

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
      select(population, new_cases, recovered, deaths)
  })  
  
  output$data2 <- renderTable({
    req(input$country2)
    continent2() %>% 
      filter(country == input$country2) %>% 
      select(population, new_cases, recovered, deaths)
  })
  
  # space for plot #
}

shinyApp(ui, server)
