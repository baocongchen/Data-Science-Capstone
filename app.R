#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2) 
library(WDI)
library(dplyr)
library(plotly)
options(warn=-1)
dat <- WDI(indicator='NY.GDP.PCAP.CD', 
           country=c('US','GB','JP','FR','DE','TH','VN','ID'), 
           start=1980, end=2014)
dat <- na.omit(dat)
countriesVector <- unique(dat$country)
countryCodesVector <- unique(dat$iso2c)
countryNames<- setNames(countryCodesVector, countriesVector)

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("LUX",
                          tabPanel('App', 
                            sidebarPanel(
                                          conditionalPanel(
                                            condition='input.dataset === "Data Table"',
                                            checkboxGroupInput('show_vars', h2('Columns in the dataset'),
                                                            choices = colnames(dat), 
                                                            selected = colnames(dat)
                                            )
                                          ),
                                          conditionalPanel(
                                                            condition='input.dataset === "Main"',
                                                            checkboxGroupInput('select_countries', label = h2('GDP Growth by Countries'),
                                                              choices = countryNames,
                                                              selected = c('VN','ID','TH')
                                                            )
                                          )
                            ),
                            mainPanel(
                              tabsetPanel(
                                id = 'dataset',
                                selected = 'Main',
                                tabPanel('Main', 
                                         plotlyOutput('myGdpPlot'),
                                         verbatimTextOutput('summary'),
                                         helpText('GDP per capita is gross domestic product 
                                                  divided by midyear population. GDP is the sum 
                                                  of gross value added by all resident producers 
                                                  in the economy plus any product taxes and minus 
                                                  any subsidies not included in the value of the 
                                                  products. It is calculated without making deductions 
                                                  for depreciation of fabricated assets or for depletion 
                                                  and degradation of natural resources. Data are in current 
                                                  U.S. dollars.')
                                ),
                                tabPanel('Data Table', DT::dataTableOutput('mytable1'))
                              )
                            ) 
                          ),
                          tabPanel('Documentation',
                                    h1('Summary'),
                                    p("This app shows World Bank's GDP Indicator. \nThe data is obtained
                                      directly from the World Bank Open Data site. Please visit ",
                                      a(href='https://data.worldbank.org/indicator', 'https://data.worldbank.org/indicator'),
                                      'for more information'), 
                                    p('The App has 2 tabs; one shows the graph of GDP per capita by year 
                                      for each country and its summary, the other the data table')
                          ),
                         fluidRow(
                           tags$footer(
                             tags$address(tags$a(href='mailto:peter_tran_cm@yahoo.com','Thong B. Tran')),
                             tags$p(tags$small('Copyright (c) 2016 RWORK All rights reserved'))
                           )
                         ),
                         tags$head(
                           tags$link(rel = "stylesheet", type = "text/css", href = "modified.css")
                         )
              )
     )
 
# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  # choose columns to display in the data table
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(dat[, input$show_vars, drop = FALSE])
  })
  
  # create reactive to select country to plot and summarize
  countries <- reactive({
    if(length(input$select_countries) > 0) {
      unlist(strsplit(input$select_countries, ' '))
    } else {
      unique(dat$iso2c)
    }
  })
  
  output$myGdpPlot <- renderPlotly({
    ggplot(filter(dat, iso2c %in% countries()), aes(year, NY.GDP.PCAP.CD, col = country)) + 
      geom_line() +     
      xlab('Year') + ylab('GDP per capita') + 
      labs(title = 'GDP Per Capita (current US$)')
  })
  
  output$summary <- renderPrint(summary(filter(dat, iso2c %in% countries())))
 
})

# Run the application 
shinyApp(ui = ui, server = server)

