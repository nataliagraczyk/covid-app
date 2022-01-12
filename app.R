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
library(data.table)
library(scales)

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

dates_df <- transpose(dates_converted)
colnames(df) <- sub("All.", "", colnames(df))
colnames(df) <- sub("dates.", "", colnames(df))

colnames(dates_df) <- df$country
rownames(dates_df) <- colnames(df[7:length(df)])

dates <- as.Date(rownames(dates_df), format = "%Y.%m.%d")

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
               mainPanel(withSpinner(plotOutput("plot")
               )
               )
             )
    ),
    tabPanel("Country comparison", 
             fluidRow(
               column(6,
                      selectInput("continent1", 
                                  "Select continent:", 
                                  choices = unique(df$continent),),
                      selectInput("country1", 
                                  "Select first country to compare:", 
                                  choices = NULL)
               ),
               column(6,
                      selectInput("continent2", 
                                  "Select continent:", 
                                  choices = unique(df$continent),
                                  selected = "Europe"),
                      selectInput("country2", 
                                  "Select second country to compare:", 
                                  choices = NULL)
               )
             ),
             fluidRow(
               withSpinner(plotOutput("plot_comparison", width = 1500, height = 600))
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
           labs(
             title = "New cases of Covid-19 yesterday",
             subtitle = "The chart shows the TOP 10 countries with the highest number of cases",
             caption = "Source: The data comes from the public API: https://mmediagroup.fr/covid-19"
           ) +
           coord_flip() +
           xlab ("Country") +
           ylab ("New Covid-19 cases")) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 13, face = "italic", hjust = 0)
      ) +
      scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3))
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
  
  c1 <- reactive({
    switch(input$country1, dates_df[,input$country1])
  })
  
  c2 <- reactive({
    switch(input$country2, dates_df[,input$country2])
  })
  
  output$plot_comparison <- renderPlot({
    
    ggplot(dates_df, aes(x=dates)) + 
      geom_line(aes(y = c1(), color = input$country1), size = 1.5) + 
      geom_line(aes(y = c2(), color = input$country2), size = 1.5) +
      labs(
        title = "Cumulative confirmed Covid-19 cases",
        subtitle = "Data from the period: 22/01/2020 - now",
        caption = "Source: The data comes from the public API: https://mmediagroup.fr/covid-19",
        color = "Country"
      ) +
      xlab ("Date") +
      ylab ("Confirmed Covid-19 cases") + 
      scale_x_date(date_breaks = "4 months",
                   date_labels = "%b %Y") +
      theme(
        axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 13, face = "italic", hjust = 0)
      ) +
      scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3))
  })
}

shinyApp(ui, server)
