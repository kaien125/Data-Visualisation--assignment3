library(readxl)
library(dplyr)
library(magrittr)
library(tidyr)
library(plotly)
library(shiny)
library(shinythemes)

impact <- read_excel("business impacts of covid-19 may 2020.xlsx", 
                     sheet = "Expected Impact of Restrictions", 
                     skip = 14)

trading <- impact %>% select(1:5)
social <- impact %>% select(1,6:9)
travel <- impact %>% select(1,10:13)

colnames(trading) <- c('Industry', 'Not at all','To a small extent', 
                       'To a moderate extent','To a great extent')
colnames(social) <- c('Industry', 'Not at all','To a small extent', 
                      'To a moderate extent','To a great extent')
colnames(travel) <- c('Industry', 'Not at all','To a small extent', 
                      'To a moderate extent','To a great extent')

trading <- trading %>% gather(key = 'impact', value = 'percentage', 
                              'Not at all','To a small extent', 
                              'To a moderate extent','To a great extent')
social <- social %>% gather(key = 'impact', value = 'percentage', 
                            'Not at all','To a small extent', 
                            'To a moderate extent','To a great extent')
travel <- travel %>% gather(key = 'impact', value = 'percentage', 
                            'Not at all','To a small extent', 
                            'To a moderate extent','To a great extent')

trading$Industry <- factor(trading$Industry)
social$Industry <- factor(social$Industry)
travel$Industry <- factor(travel$Industry)

trading$impact <- factor(trading$impact,levels = c('Not at all', 
                                                   'To a small extent', 
                                                   'To a moderate extent', 
                                                   'To a great extent'), ordered = TRUE)
social$impact <- factor(social$impact,levels = c('Not at all', 
                                                 'To a small extent', 
                                                 'To a moderate extent', 
                                                 'To a great extent'), ordered = TRUE)
travel$impact <- factor(travel$impact,levels = c('Not at all', 
                                                 'To a small extent', 
                                                 'To a moderate extent', 
                                                 'To a great extent'), ordered = TRUE)

industry <-  unique(trading$Industry) 

restriction_set <- list(trading, social, travel)

restriction_list <- c('Restrictions on Trading' , 
                      'Social Distancing Restrictions', 
                      'Travel Restrictions')

ui <- fluidPage(theme = shinytheme("spacelab"),
  titlePanel("Business Impacts of COVID-19 Survey, May 2020"),
  h4('Expected impact of government restrictions in the next 2 months'),
  sidebarLayout(
    sidebarPanel(
      p('Select an industry from the dropdown list.'),
      
      selectInput('Industry', 'Industry', choices = industry),
      
      p('Select a restriction from the dropdown list.'),
      
      selectInput('restriction', 'Government Restriction',
                  choices =c('Restrictions on Trading' = '1', 
                             'Social Distancing Restrictions'='2',
                             'Travel Restrictions' = '3')),
      
      p(strong('Data collection')),
      
      p('The collection was conducted through a telephone based business 
      survey between 13 May and 22 May 2020, with a sample size of 2,564 businesses. 
      The final response rate was 56% (1,430 responding businesses).'),
    
      p(strong('Reliability of the estimates')),
    
    p('Estimates of the prevalence of adversely affected 
    businesses may be an underestimate if 
    businesses have typically not responded because they 
      have been adversely affected by COVID-19.'),),
    
    mainPanel(
      p('Hover mouse cursor over a bar to display the detail.'),
              
              plotlyOutput("tradingPlot"),br(),
              
              p('Data Source and Reference: Australian Bureau of Statistics,', 
              em('Business Impacts Of COVID-19, 
                May 2020.'), '[online] Available at:',
                 
                a(em('<https://www.abs.gov.au/ausstats/abs@.nsf/mf/5676.0.55.003>'),
                  href = 'https://www.abs.gov.au/ausstats/abs@.nsf/mf/5676.0.55.003', 
                  target="_blank"),
                ' [Accessed 9 June 2020].'))
  )
)

server <- function(input, output) {
  datasetInput <- reactive({
    temp <- data.frame(restriction_set[[as.numeric(input$restriction)]])
  })
  
  output$tradingPlot <- renderPlotly({
    data1 <- datasetInput()
    
    plot_ly(data = filter(data1, Industry == input$Industry), 
            x = ~impact, y = ~percentage, type = 'bar', color = I("#a6bddb"), alpha = 0.8,
            hovertemplate = paste("",filter(data1, Industry == input$Industry)$percentage,
                                  "% of businesses in<br>",
                                  tolower(input$Industry),"industry are<br>", 
                                  tolower(filter(data1, Industry == input$Industry)$impact),
                                  "<br> expected to be impacted by<br>", 
                                  tolower(restriction_list[as.numeric(input$restriction)]),
                                  '<extra></extra>')) %>%   
      
      layout(xaxis = list(zeroline = FALSE, title = "Extent of Impact"),
             yaxis = list( title = "Percentage of the Industry",ticksuffix = "%") ) 
  })
}

shinyApp(ui = ui, server = server)
