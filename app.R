library(httr)
library(jsonlite)
library(shiny)
library(tidyverse)
library(dplyr)
library(shinycssloaders)

# Getting data from an API

# statistics data set

stat_url <- "https://covid-193.p.rapidapi.com/statistics"

res_stat <- GET(stat_url, 
                add_headers(
                  `x-rapidapi-host` = 'covid-193.p.rapidapi.com', 
                  `x-rapidapi-key` = Sys.getenv("ennvar_covid")
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
                  `x-rapidapi-key` = Sys.getenv("ennvar_covid"), 
                query = queryString
)


hist_data <- jsonlite::fromJSON(rawToChar(res_hist$content))
hist_df <- as.data.frame(hist_data$response)


# Data manipulation

df <- stat_df[ !is.na(stat_df$population) == TRUE, ]

df$new_cases <- as.numeric(df$cases$new)

variable <- names(df)

# Shiny App

library(shiny)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel(
    "Covid-19 App"
  ),
  navlistPanel(
    tabPanel("Start", 
             sidebarLayout(
               sidebarPanel(
                 textOutput("cases"),
                 actionButton("new", "New cases", class = "btn-warning"),
                 actionButton("deaths", "Deaths", class = "btn-warning"),
                 actionButton("recovered", "Recovered", class = "btn-warning")
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
    tabPanel("Historical data", "Tutaj będą dane historyczne"),
    tabPanel("Country comparison", "Tutaj będzie można porównać wskaźniki dla dwóch lub więcej państw")
  )
)

server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  output$cases <- renderText(
    paste("Select a button")
  )
  
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
}

shinyApp(ui, server)


