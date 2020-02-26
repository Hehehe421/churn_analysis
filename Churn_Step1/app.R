#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(plotly)
library(data.table)
library(forecast)
library(zoo)
library(tidyverse)
library(tseries)
library(lubridate)
library(anytime)
library(gridExtra)
library(prophet)

churn = fread("./WA_Fn-UseC_-Telco-Customer-Churn.csv")
cat = fread("./cat.csv")
num = fread("./num.csv")

tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                 height: 150px;
                                 -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 3;    /* Firefox */ 
                                 column-count: 3; 
                                 -moz-column-fill: auto;
                                 -column-fill: auto;
                                 } 
                                 ")) 
  ))

num_cols = c('SeniorCitizen', 'tenure', 'MonthlyCharges', 'TotalCharges')

category_choices = c("gender","Partner" ,"Dependents","PhoneService", "MultipleLines" ,
                     "InternetService","OnlineSecurity","OnlineBackup","DeviceProtection",
                     "TechSupport","StreamingTV","StreamingMovies","Contract",
                     "PaperlessBilling","PaymentMethod")

category_selected <- c("Contract")

plot_groupbar <- function(data, group1, group2, pos, title, xlable){
  p = data %>% group_by(data[[group1]], data[[group2]]) %>%
    summarise(count=n()) %>%
    mutate(pct=count/sum(count))%>%
    ggplot(aes(x=`data[[group2]]`, y=count, colour=`data[[group2]]`, fill=`data[[group2]]`)) +
    geom_bar(stat="identity") +
    facet_grid(.~`data[[group1]]`, labeller = as_labeller(c(`Yes`='Churned', `No` = 'Non-Churned'))) +
    scale_y_continuous() + 
    geom_text(aes(label=paste0(round(pct*100,1),"%"),
                  y=count+pos), size=4) +
    ggtitle(title) + xlab(xlable) +
    theme_bw() + theme(legend.title = element_blank()) +
    theme(
      strip.background = element_rect(
        color="black", fill="orange", size=1.5, linetype="solid"
      ),
      axis.text = element_text( size = 14 ),
      axis.text.x = element_text( size = 14,angle = 45, hjust = 1),
      axis.title = element_text( size = 16, face = "bold" ),
      legend.position="none",
      # The new stuff
      strip.text = element_text(size = 20)
    )
  return (p)
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Churn Dashboard",
                  titleWidth = 450),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Data Summary", tabName = "dashboard1", icon = icon("dashboard")),
    menuItem("Categorical Variables", tabName = "dashboard2", icon = icon("th")),
    menuItem("Numerical Variables", tabName = "dashboard3", icon = icon("th")),
    menuItem("Feature Engineer", tabName = "dashboard4", icon = icon("th")),
    menuItem("Conclusion", tabName = 'dashboard5', icon = icon("th"))#,
    #menuItem('Test', tabName = 'test', icon = icon("th"))
   )
  #,
  # dateRangeInput("daterange1", "Date range:",
  #                start = "2016-05-01",
  #                end   = "2018-06-30")
  
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'dashboard1',
              fluidRow(tweaks,
                       column(width = 4,
                              box(width = 12,height = 400,
                                  title = "Categorical Variables Summary", status = "primary", solidHeader = TRUE,
                                  DTOutput('tb1')
                                  )
                                  
                              ),
                       column(width = 5,
                              box(width = 12,height = 400,
                                  title = "Numerical Variables Summary", status = "primary", solidHeader = TRUE,
                                  DTOutput('tb2')
                                  
                              )
                              
                       ),
                       column(width = 3,
                              box(width = 12, height = 400,
                                  title = 'Churn Distribution Overall', status = 'warning', solidHeader = TRUE,
                                  plotlyOutput('plot1', height = 345)
                                  )
                              )
              ),
              fluidRow(
                column(width = 12,
                       box(width = 12,height = 380,
                           title = "Head of Dataset", status = "primary", solidHeader = TRUE,
                           DTOutput('tb3')
                          )
                       ))
      ),
      tabItem(tabName = 'dashboard2',
              fluidRow(tweaks,
                       column(width = 7,
                              box(width = 12,height = 615,
                                  title = "Bar Plot within Category by Churn", status = "primary", solidHeader = TRUE,
                                  plotOutput('plot2')
                              )
                              
                       ),
                       column(width = 5,
                              box(width = 12,height = 250,
                                  title = "Select the Categorical Variables", status = "primary", solidHeader = TRUE,
                                  tags$div(align = 'left', 
                                           class = 'multicol', 
                                           radioButtons("category_input1","Check Below:",category_choices, selected = category_selected))
                                  
                                  
                              ),
                              box(width = 12, height = 345,
                                  title = "Table View",status = "primary", solidHeader = TRUE,
                                  DTOutput('tb4')
                                  )
                              
                       )
              )
              
              
      ),
      
      tabItem(tabName = 'dashboard3'
              ),
      tabItem(tabName = 'dashboard4'
              
      ),
      tabItem(tabName = 'dashboard5'
              )
    )
  )
  
  )

server <- function(input, output) {
  
  my_tb4 <- reactive({
    df = churn[,{
      count = .N
      .SD[,.(Count = .N,Percentage=paste0(round(.N/count*100,1),"%")),by=eval(input$category_input1)]
    },by=Churn]
    df
  }) 
  
  output$plot1 = renderPlotly(
    churn %>%
      group_by(Churn) %>%
      dplyr::summarize(count=n()) %>%
      plot_ly(labels = ~Churn, values = ~count, marker = list(colors = c("deepskyblue", "tomato"))) %>%
      add_pie(hole = 0.5) %>%
      layout(showlegend = T,
             #title = 'Customer Churn Distribution',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
  

  output$tb1 = DT::renderDataTable(
    cat[, c(1,3,4,6)],
    rownames = FALSE,
    options = list(autoWidth = TRUE,searching = FALSE, paging = FALSE)
  )
    
  output$tb2 = DT::renderDataTable(
    num,
    rownames = FALSE,
    options = list(autoWidth = TRUE,searching = FALSE, paging = FALSE)
  )
  
  output$tb3 = DT::renderDataTable(
    head(churn,5),
    rownames = FALSE,
    options = list(autoWidth = TRUE,searching = FALSE, paging = FALSE, scrollX = TRUE)
  )
  
  output$plot2 = renderPlot(height = 500,
    plot_groupbar(churn, "Churn", c(input$category_input1), 100, 
                  "", 
                  c(input$category_input1))
  )
  
  output$tb4 = DT::renderDataTable(
    my_tb4(),
    rownames = FALSE,
    options = list(autoWidth = TRUE,searching = FALSE, paging = FALSE)
  )
  
  
  
  
  
}

shinyApp(ui, server)

