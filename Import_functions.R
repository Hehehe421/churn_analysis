library(plyr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(DT)
library(data.table)
library(anytime)
library(tidyverse)
library(zoo)
library(Hmisc)
library(ggplot2)
library(plotly)

churn = fread("./Data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

describe(churn)

cols1 <-
  c(
    Yes = "olivedrab",
    No = "tomato"
  )

churn %>%
  group_by(Churn) %>%
  dplyr::summarize(count=n()) %>%
  plot_ly(labels = ~Churn, values = ~count, marker = list(colors = c("deepskyblue", "tomato"))) %>%
  add_pie(hole = 0.5) %>%
  layout(showlegend = T,
         title = 'Customer Churn Distribution',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

churn %>%
  group_by(Churn, gender) %>%
  dplyr::summarise(count=n()) %>%
  ggplot() + facet_wrap(~Churn) + geom_bar(aes(x = gender, y = count, fill = gender), stat = "identity")



ggplot(churn, aes(x=as.factor(Churn), fill=as.factor(gender)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  ylab('Percent of Gender Group, %') +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()



ggplot(churn, aes(x=as.factor(Churn), fill=as.factor(gender)))+
  geom_bar(aes( y=..count.., position="dodge" )) +
  geom_text(aes( y=..count.., label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  ylab('Percent of Gender Group, %') +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggplot(mtcars, aes('factor(cyl)', fill='factor(cyl)'))
  + geom_bar()
  + geom_text(
    aes(label='stat(count)'),
    stat='count',
    nudge_x=-0.14,
    nudge_y=0.125,
    va='bottom'
  ) 

mtcars %>%
  mutate(gear = factor(gear)) %>%
  group_by(gear, cyl) %>%
  count() %>%
  group_by(gear) %>%
  mutate(percentage = n/sum(n)) %>%
  ggplot(aes(x = cyl, y = percentage, fill = gear)) +
  geom_bar(position = 'dodge', stat = 'identity')


library(scales)
set.seed(25)
test <- data.frame(
  test1 = sample(letters[1:2], 100, replace = TRUE), 
  test2 = sample(letters[3:8], 100, replace = TRUE)
)

labeli <- function(variable, value){
  names_li <- list("Yes"="Churned", "No"="Non-Churned")
  return(names_li[value])
}

# Summarize to get counts and percentages
test.pct = churn %>% group_by(Churn, gender) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count)) 

p = ggplot(test.pct, aes(x=gender, y=count, colour=gender, fill=gender)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ Churn, labeller = as_labeller(c(`Yes`='Churned', `No` = 'Non-Churned'))) +
  scale_y_continuous() + 
  geom_text(data=test.pct, aes(label=paste0(round(pct*100,1),"%"),
                               y=count+100), size=4) +
  ggtitle("Gender Vs Churn Distribution") + 
  theme_bw()

plot_groupbar <- function(data, group1, group2, pos){
  p = data %>% group_by(data[[group1]], data[[group2]]) %>%
    summarise(count=n()) %>%
    mutate(pct=count/sum(count)) %>%
    ggplot(aes(x=data[[group2]], y=data[[count]], colour=data[[group2]], fill=data[[group2]])) +
    geom_bar(stat="identity") +
    facet_grid(. ~ data[[group1]], labeller = as_labeller(c(`Yes`='Churned', `No` = 'Non-Churned'))) +
    scale_y_continuous() + 
    geom_text(aes(label=paste0(round(pct*100,1),"%"),
                                 y=count+pos), size=4) +
    ggtitle("Gender Vs Churn Distribution") + 
    theme_bw()
  
  return (p)
}


churn %>% group_by(Churn, gender) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  ggplot(aes(x=gender, y=count, colour=gender, fill=gender)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ Churn, labeller = as_labeller(c(`Yes`='Churned', `No` = 'Non-Churned'))) +
  scale_y_continuous() + 
  geom_text(data=test.pct, aes(label=paste0(round(pct*100,1),"%"),
                               y=count+100), size=4) +
  ggtitle("Gender Vs Churn Distribution") + 
  theme_bw()

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
      )
    )
    return (p)
}


###summary table
t = summary(churn)
t = as.data.frame(t)

churn[,{
  count = .N
  .SD[,.(Count = .N,Percentage=paste0(round(.N/count*100,1),"%")),by=gender]
},by=Churn]


num_cols <- c('SeniorCitizen', 'tenure', 'MonthlyCharges', 'TotalCharges')  

summary(churn[, num_cols])
