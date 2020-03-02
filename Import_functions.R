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

p2 <- ggplot(churn, aes(x=Churn, y=tenure, fill = Churn)) + 
  geom_boxplot() +
  facet_wrap(~Churn, scale="free", labeller = as_labeller(c(`Yes`='Churned', `No` = 'Non-Churned')))+
  theme_bw() + theme(legend.title = element_blank()) +
  theme(
    strip.background = element_rect(
      color="black", fill="orange", size=1.5, linetype="solid"
    ))
p2

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
      )
    )
  return (p)
}
plot_groupbox(churn, "Churn", "tenure","e", "e")


plot_groupbox <- function(data, group1, group2, fill=NULL){
  p = ggplot(data, aes_string(x=group1, y=group2, fill = fill)) +
    geom_boxplot() +
    facet_wrap(~Churn, labeller = as_labeller(c(`Yes`='Churned', `No` = 'Non-Churned')))
  return (p)
}

plot_groupbox(churn, "Churn", "tenure","Churn")


# Interleaved histograms
ggplot(churn, aes(x=tenure, color=Churn)) +
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")
# Add mean lines
mu <- ddply(churn, "Churn", summarise, grp.mean=mean(tenure))
p<-ggplot(churn, aes(x=tenure, color=Churn, fill=Churn)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Churn),
             linetype="dashed")+
  theme(legend.position="top")
p

plot_grouphist <- function(data, group1, title, xlable){
  mu <- churn[, list(mean = mean(data[[group1]])), by = Churn]
  p<-ggplot(data, aes_string(x=group1, color="Churn", fill="Churn")) +
    geom_histogram(position="identity", alpha=0.5, bins = 30)+
    geom_vline(data=mu, aes(xintercept=mean, color=Churn),
               linetype="dashed")+
    ggtitle(title) + xlab(xlable) +
    theme_bw() + theme(legend.title = element_blank()) +
    theme(
      strip.background = element_rect(
        color="black", fill="orange", size=1.5, linetype="solid"
      )
    )
  return (p)
}

plot_grouphist(churn, "tenure", '', '')

library("factoextra")
df_churn = churn[, -21]
res.pca <- prcomp(df_churn, scale = TRUE)

df = churn[, tenure_group := cut(tenure, breaks = 6, 
                                 labels = c('tenure_1yr', 'tenure_2yr', 'tenure_3yr', 'tenure_4yr', 'tenure_5yr', 'tenure_6yr'))][
                                   , monthly_group := cut(MonthlyCharges, 
                                                          breaks=quantile(MonthlyCharges, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                                                          include.lowest=TRUE, labels = c("Monthly_Q1", "Monthly_Q2", "Monthly_Q3", "Monthly_Q4"))
                                 ][, total_group := cut(TotalCharges, 
                                                          breaks=quantile(TotalCharges, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                                                          include.lowest=TRUE, labels = c("Total_Q1", "Total_Q2", "Total_Q3", "Total_Q4"))
                                   
                                 ]

plot_groupbar(df, "Churn", "monthly_group", 100, "f", "t")

my.data = churn[, .(Count = .N), by = c( 'tenure_group','Contract', 'Churn')]
SankeyDiagram(my.data[, -4],
              link.color = "Source", 
              label.show.varname = FALSE,
              weights = my.data$Count) 

data = churn[, .(Count = .N), by = c( 'tenure_group','monthly_group', 'Churn') ]
ggplot(data, aes(x = tenure_group, y = Count, fill = monthly_group)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ Churn)
SankeyDiagram(data[, -4],
              link.color = "Source", 
              label.show.varname = FALSE,
              weights = data$Count) 

t = mutate_if(df, is.character, ~ as.numeric(as.character(.x)))

char_columns <- sapply(churn, is.character)             # Identify character columns
data_chars_as_num <- churn                              # Replicate data
data_chars_as_num[ , char_columns] <- as.data.frame(   # Recode characters as numeric
  apply(data_chars_as_num[ , char_columns], 2, as.numeric))
sapply(data_chars_as_num, class)  


num_churn = churn2[, c('tenure_group', 'monthly_group', 'total_group', 'Churn')]
churn2 = na.omit(churn)
ggparcoord(num_churn,
           columns = 1:3, groupColumn = 4
) 
churn2[, c("tenure_group")]<- sapply(churn2[, c("tenure_group")], as.numeric)
churn2[, c("monthly_group")]<- sapply(churn2[, c("monthly_group")], as.numeric)
churn2[, c("total_group")]<- sapply(churn2[, c("total_group")], as.numeric)
churn2[, c("gender")]<- sapply(churn2[, c("gender")], as.numeric)

DF <- data.frame("a" = as.character(0:5),
                 "b" = paste(0:5, ".1", sep = ""),
                 "c" = letters[1:6],
                 stringsAsFactors = FALSE)

# Check columns classes
sapply(DF, class)

#           a           b           c 
# "character" "character" "character" 

cols.num <- c("a","b")
DF[cols.num] <- sapply(DF[cols.num],as.numeric)
sapply(DF, class)


level1 <- c("Yes", "No", "No internet service")
churn$OnlineBackup <- as.integer(factor(churn$OnlineBackup, levels = level1))
str(churn$OnlineBackup)
category_choices = c("gender","Partner" ,"Dependents","PhoneService", "MultipleLines" ,
                     "InternetService","OnlineSecurity","OnlineBackup","DeviceProtection",
                     "TechSupport","StreamingTV","StreamingMovies","Contract",
                     "PaperlessBilling","PaymentMethod", "SeniorCitizen")

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
# matrix of the p-value of the correlation
p.mat <- cor.mtest(churn_num[, c(-1, -24, -23, -22)])
head(p.mat[, 1:5])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", 
         order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         number.cex=0.5,
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
