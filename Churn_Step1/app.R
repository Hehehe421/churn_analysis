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
library(flipPlots)
library(GGally)
library(networkD3)
library(corrplot)

churn = fread("./churn.csv")
cat = fread("./cat.csv")
num = fread("./num.csv")

churn <- na.omit(churn)

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


num_choices = c('tenure', 'MonthlyCharges', 'TotalCharges')

category_choices = c("gender","Partner" ,"Dependents","PhoneService", "MultipleLines" ,
                     "InternetService","OnlineSecurity","OnlineBackup","DeviceProtection",
                     "TechSupport","StreamingTV","StreamingMovies","Contract",
                     "PaperlessBilling","PaymentMethod", "SeniorCitizen")

category_selected <- c("Contract")
num_selected <- c("tenure")
churn = churn[, tenure_group := cut(tenure, breaks = 6, 
                                 labels = c('tenure_1yr', 'tenure_2yr', 'tenure_3yr', 'tenure_4yr', 'tenure_5yr', 'tenure_6yr'))][
                                   , monthly_group := cut(MonthlyCharges, 
                                                          breaks=quantile(MonthlyCharges, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                                                          include.lowest=TRUE, labels = c("Monthly_Q1", "Monthly_Q2", "Monthly_Q3", "Monthly_Q4"))
                                   ][, total_group := cut(TotalCharges, 
                                                          breaks=quantile(TotalCharges, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                                                          include.lowest=TRUE, labels = c("Total_Q1", "Total_Q2", "Total_Q3", "Total_Q4"))
                                     
                                     ]
#my.data = churn[, .(Count = .N), by = c('tenure_group','Contract', 'Churn')]
churn_num <- churn
level1 <- c("Yes", "No","No internet service")
churn_num$OnlineBackup <- as.integer(factor(churn_num$OnlineBackup, levels = level1))
churn_num$OnlineSecurity <- as.integer(factor(churn_num$OnlineSecurity, levels = level1))
churn_num$DeviceProtection <- as.integer(factor(churn_num$DeviceProtection, levels = level1))
churn_num$TechSupport <- as.integer(factor(churn_num$TechSupport, levels = level1))
churn_num$StreamingTV <- as.integer(factor(churn_num$StreamingTV, levels = level1))
churn_num$StreamingMovies <- as.integer(factor(churn_num$StreamingMovies, levels = level1))

level2 <- c ("Month-to-month", "One year", "Two year")
churn_num$Contract <- as.integer(factor(churn_num$Contract, levels = level2))

level3 <- c("Yes", "No")
churn_num$Partner <- as.integer(factor(churn_num$Partner, levels = level3))
churn_num$Dependents <- as.integer(factor(churn_num$Dependents, levels = level3))
churn_num$PhoneService <- as.integer(factor(churn_num$PhoneService, levels = level3))
churn_num$PaperlessBilling <- as.integer(factor(churn_num$PaperlessBilling, levels = level3))
#churn_num$Churn <- as.integer(factor(churn_num$Churn, levels = level3))
churn_num[,":="(Churn=ifelse(Churn=="No",0,1))]

level4 <- c("Yes", "No", "No phone service")
churn_num$MultipleLines <- as.integer(factor(churn_num$MultipleLines, levels = level4))

level5 <- c( "Electronic check","Mailed check","Bank transfer (automatic)","Credit card (automatic)")
churn_num$PaymentMethod <- as.integer(factor(churn_num$PaymentMethod, levels = level5))

level6 <- c("Female", "Male")
churn_num$gender <- as.integer(factor(churn_num$gender, levels = level6))

level7 <- c("DSL", "Fiber optic", "No")
churn_num$InternetService <- as.integer(factor(churn_num$InternetService, levels = level7))

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

layer1_choices = c(
  "InternetService","StreamingTV","Contract",
  "PaymentMethod","tenure_group", 
  "monthly_group", "total_group")

layer2_choices = c(
  "InternetService","StreamingTV","Contract",
  "PaymentMethod","tenure_group", 
  "monthly_group", "total_group")
layer1_selected = c('Contract')
layer2_selected = c('tenure_group')

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

plot_groupbox <- function(data, Churn, group2, title, xlable){
  p = ggplot(data, aes_string(x=Churn, y=group2, fill = Churn)) +
    geom_boxplot() +
    facet_wrap(~Churn, scale="free",labeller = as_labeller(c(`Yes`='Churned', `No` = 'Non-Churned'))) +
    #scale_y_continuous() + 
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

plot_grouphist <- function(data, group1, title, xlable){
  
  p<-ggplot(data, aes_string(x=group1, color="Churn", fill="Churn")) +
    geom_histogram(position="identity", alpha=0.7, bins = 30)+
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
  dashboardHeader(title = "Step1 Understand the Dataset",
                  titleWidth = 450),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Data Summary", tabName = "dashboard1", icon = icon("dashboard")),
    menuItem("Categorical Variables", tabName = "dashboard2", icon = icon("th")),
    menuItem("Numerical Variables", tabName = "dashboard3", icon = icon("th")),
    menuItem("Feature Engineer", tabName = "dashboard4", icon = icon("th")),
    menuItem("Conclusion", tabName = 'dashboard5', icon = icon("th")),
    menuItem('R code', tabName = 'dashboard6', icon = icon("th"))
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
                       column(width = 12,
                box(width = 12,height = 350,
                    title = "Project Summary", status = "success", solidHeader = TRUE,
                    includeMarkdown("finding1.Rmd")
                    
                ))
                
              ),
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
                                  DTOutput('tb2'),
                                  textOutput("text1")
                                  
                              )
                              
                       ),
                       column(width = 3,
                              box(width = 12, height = 400,
                                  title = 'Churn Distribution Overall', status = 'warning', solidHeader = TRUE,
                                  plotlyOutput('plot1', height = 325),
                                  textOutput("text2")
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
              ),
              fluidRow(tweaks,
                       column(width = 12,
                       box(width = 12,height = 250,
                           title = "Summary", status = "success", solidHeader = TRUE,
                           includeMarkdown("finding2.Rmd")
                           
                       ))
                       
              )              
              
              
      ),
      
      tabItem(tabName = 'dashboard3',
              fluidRow(tweaks,
                       column(width = 7,
                              box(width = 12,height = 615,
                                  title = "Box Plot within Numerical Vars by Churn", status = "primary", solidHeader = TRUE,
                                  plotOutput('plot3')
                              )
                              
                       ),
                       column(width = 5,
                              box(width = 12,height = 180,
                                  title = "Select the Numerical Variables", status = "primary", solidHeader = TRUE,
                                  tags$div(align = 'left', 
                                           class = 'multicol', 
                                           radioButtons("num_input1","Check Below:",num_choices, selected = num_selected))
                                  
                                  
                              ),
                              box(width = 12, height = 415,
                                  title = "Histogram by Churn",status = "warning", solidHeader = TRUE,
                                  plotOutput('plot4')
                              )
                              
                       )
              ),
              fluidRow(tweaks,
                       column(width = 12,
                       box(width = 12,height = 300,
                           title = "Summary", status = "success", solidHeader = TRUE,
                           includeMarkdown("finding3.Rmd")
                           
                       )
                       )
                       
              )              
              ),
      tabItem(tabName = 'dashboard4',
              fluidRow(#tweaks,
                       column(width = 5,
                              box(width = 12,height = 315,
                                  title = "Convert Numerical into Categorical", status = "primary", solidHeader = TRUE,
                                  includeMarkdown("finding4.Rmd")
                                  
                                  
                              ),
                              box(width = 6,
                                  title = "Sankey first layer", status = "warning", solidHeader = TRUE,
                                  #tags$div(align = 'left', 
                                         #class = 'multicol',
                                         radioButtons("layer_1","Check below:",layer1_choices, selected = layer1_selected)#)
                              ),
                              box(width = 6,
                                  title = "Sankey second layer", status = "warning", solidHeader = TRUE,
                                  #tags$div(align = 'left', 
                                         #class = 'multicol',
                                         radioButtons("layer_2","Check below:",layer2_choices, selected = layer2_selected)#)
                              )
                              
                       ),
                       column(width = 7,
                              box(width = 12,height = 615,
                                  title = "Sankey Flow by cluster", status = "warning", solidHeader = TRUE,
                                  sankeyNetworkOutput("plot_5")
                                  
                              )
                              
                       )
              ),
              fluidRow(tweaks,
                       column(width = 5,
                              box(width = 12, height = 665,
                                  title = "Correlation Analysis", status = "success", solidHeader = TRUE,
                                  includeMarkdown('finding5.Rmd')
                                  )
                              ),
                       column(width = 7,
                              box(width = 12, height = 665,
                                  title = "Correlation Plot with Converted Numeric", status = "success", solidHeader = TRUE,
                                  plotOutput("plot_6", height = 600)
                                  )
                       
                              )
                       )
              
      ),
      tabItem(tabName = 'dashboard5',
              fluidRow(tweaks,
                       column(width = 12,
                              box(width = 12, height = 860,
                                  title = "Conclusions", status = "success", solidHeader = TRUE,
                                  includeMarkdown("finding6.Rmd")
                                  )
                              )
                       
                       )
              ),
      tabItem(tabName = 'dashboard6',
              fluidRow(tweaks,
                       column(width = 12,
                              tabBox(width = 12,
                                     title = 'R code for the dashboard',
                                     
                                     side = 'right', selected = 'app',
                                     tabPanel('app', "UI.R", includeMarkdown("ui.Rmd")),
                                     tabPanel('server', "Server.R", includeMarkdown("server.Rmd")),
                                     tabPanel('helper', "Helper_function.R", includeMarkdown("helper_function.Rmd"))
                              )
                       )
                       
              )
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
  
  my_data <- reactive({
    df = churn[, .(Count = .N), by = eval(c(input$layer_1,input$layer_2, 'Churn'))]
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
  output$plot3 = renderPlot(height = 500,
    plot_groupbox(churn, "Churn", input$num_input1, 
                  "",
                  "")
  )
  
  output$plot4 = renderPlot(height = 360,
    plot_grouphist(churn,input$num_input1, '', '')
                            
  )
  
  output$tb4 = DT::renderDataTable(
    my_tb4(),
    rownames = FALSE,
    options = list(autoWidth = TRUE,searching = FALSE, paging = FALSE)
  )
  
  output$plot_5 = renderSankeyNetwork({
    my.data = my_data()
     SankeyDiagram(my.data[, -4],
                   link.color = "Source", 
                   label.show.varname = FALSE,
                   weights = my.data$Count) 
                            } )
  
  output$plot_6 = renderPlot({
    cor.mtest <- function(mat, ...) {
      mat <- as.matrix(mat)
      n <- ncol(mat)
      p.mat<- matrix(NA, n, n)
      diag(p.mat) <- 0
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          tmp <- cor.test(mat[, i], mat[, j], ...)
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
      }
      colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
      p.mat
    }
    M <- cor(churn_num[, c(-1, -24, -23, -22)])
    p.mat <- cor.mtest(churn_num[, c(-1, -24, -23, -22)])
    #head(p.mat[, 1:5])
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corrplot(M, method="color", col=col(200),  
             type="upper", 
             order="hclust", 
             addCoef.col = "black", # Add coefficient of correlation
             number.cex=0.75,
             tl.col="black", tl.srt=45, #Text label color and rotation
             # Combine with significance
             p.mat = p.mat, sig.level = 0.01, insig = "blank", 
             # hide correlation coefficient on the principal diagonal
             diag=FALSE 
    )
    
  })
  output$text1 <- renderText({
    HTML(
    paste(
          "There are 11 missing values in TotalCharges, we will remove these customer in future analysis."
          )
    )
  })
  
  output$text2 <- renderText({
    HTML(
      paste(
        "The ratio of (Target = 'No')/(Target = 'Yes') is 2.7."
      )
    )
  })  
  
  
  
}

shinyApp(ui, server)

