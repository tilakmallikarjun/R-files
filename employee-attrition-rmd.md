Employee attrition
================
Mallikarjun Tilak
1/9/2022

``` r
##beginning of code
```

``` r
knitr::opts_chunk$set(fig.path='Figs/')
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

Installing required libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(mice)
```

    ## 
    ## Attaching package: 'mice'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following objects are masked from 'package:base':
    ## 
    ##     cbind, rbind

``` r
library(dplyr)
library(ggcorrplot)
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(ggplot2)
library(rpart)
library(rpart.plot)
library(tree)
```

    ## Registered S3 method overwritten by 'tree':
    ##   method     from
    ##   print.tree cli

``` r
library(ggthemes)
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(xgboost)
```

    ## 
    ## Attaching package: 'xgboost'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

``` r
library(knitr)
opts_chunk$set(dev="png")
```

Loading the data and checking for null or NA values

``` r
train_data<-read.csv("train_MpHjUjU.csv")
nas<- lapply(train_data, function(x){length(which(is.na(x)))})
blanks<- lapply(train_data, function(x){length(which(x==""))})
```

Date formatting of the variables associated with date

``` r
train_data$MMM.YY <- as.Date(train_data$MMM.YY, format= "%Y-%m-%d")
train_data$LastWorkingDate <- as.Date(train_data$LastWorkingDate, format= "%Y-%m-%d")
train_data$Dateofjoining <- as.Date(train_data$Dateofjoining, format= "%Y-%m-%d")
train_data$DaysInCompany<-train_data$MMM.YY-train_data$Dateofjoining
```

#encoding target variable

``` r
train_data$lyd <- ifelse(is.na(train_data$LastWorkingDate), "No", "Yes")

attrition_data<-subset(train_data, lyd == "Yes")
lyd_data<-select(attrition_data, c(Emp_ID, LastWorkingDate))
```

``` r
##use merge"
train_data<-merge(train_data,lyd_data,by="Emp_ID" ,all.x = TRUE)
train_data$daysb4lastworkingdate<-train_data$LastWorkingDate.y- train_data$MMM.YY
```

``` r
###Encoding attrition Risk##
train_data$attrition<-ifelse(train_data$daysb4lastworkingdate < 180, "Yes", "No")
train_data$attrition[is.na(train_data$attrition)]<- "No"
```

``` r
train_data$DaysInCompany[train_data$DaysInCompany<0]<-0
train_data$attrition<-as.factor(train_data$attrition)
```

``` r
cols<- c( "Gender", "City","Education_Level")

train_data[cols]<-lapply(train_data[cols], factor)
#split date
train_data$date<-train_data$MMM.YY
train_data<-train_data%>% separate(date, sep="-",into=c("year","month", "day") )
train_data$month<- as.numeric(train_data$month)

#train_data$pastquarter<- train_data$month -3
#past_quarter_data<-select(train_data, c("Emp_ID","Quarterly.Rating","month"))

#names(past_quarter_data)[names(past_quarter_data) == 'month'] <- 'pastquarter'
#train_data<-merge(train_data,past_quarter_data,  by=c("Emp_ID", "pastquarter"))
head(train_data)
```

    ##   Emp_ID     MMM.YY Age Gender City Education_Level Salary Dateofjoining
    ## 1      1 2016-03-01  28   Male  C23          Master  57387    2015-12-24
    ## 2      1 2016-01-01  28   Male  C23          Master  57387    2015-12-24
    ## 3      1 2016-02-01  28   Male  C23          Master  57387    2015-12-24
    ## 4      2 2017-11-01  31   Male   C7          Master  67016    2017-11-06
    ## 5      2 2017-12-01  31   Male   C7          Master  67016    2017-11-06
    ## 6      4 2017-03-01  43   Male  C13          Master  65603    2016-12-07
    ##   LastWorkingDate.x Joining.Designation Designation Total.Business.Value
    ## 1        2016-03-11                   1           1                    0
    ## 2              <NA>                   1           1              2381060
    ## 3              <NA>                   1           1              -665480
    ## 4              <NA>                   2           2                    0
    ## 5              <NA>                   2           2                    0
    ## 6              <NA>                   2           2               350000
    ##   Quarterly.Rating DaysInCompany lyd LastWorkingDate.y daysb4lastworkingdate
    ## 1                2       68 days Yes        2016-03-11               10 days
    ## 2                2        8 days  No        2016-03-11               70 days
    ## 3                2       39 days  No        2016-03-11               39 days
    ## 4                1        0 days  No              <NA>               NA days
    ## 5                1       25 days  No              <NA>               NA days
    ## 6                1       84 days  No        2017-04-27               57 days
    ##   attrition year month day
    ## 1       Yes 2016     3  01
    ## 2       Yes 2016     1  01
    ## 3       Yes 2016     2  01
    ## 4        No 2017    11  01
    ## 5        No 2017    12  01
    ## 6       Yes 2017     3  01

## USE different DATA for test

``` r
test<-read.csv("test_hXY9mYw.csv")

testfull_df<-subset(train_data, Emp_ID %in% test$Emp_ID)
#test_df<-distinct(testfull_df,Emp_ID)
#testfull_df$DaysInCompany<-testfull_df$DaysInCompany+180
```

``` r
uniq_Emp_id <- testfull_df %>% group_by(Emp_ID) %>% summarise(Total.Business.Value = sum(Total.Business.Value))
testfull_df["Total.Business.Value"]<- NULL
merged<-merge(testfull_df, uniq_Emp_id, by= "Emp_ID")
merged$MMM.YY <- as.Date(merged$MMM.YY, format= "%Y-%m-%d")
test_df<-merged %>% 
  group_by(Emp_ID) %>%
  dplyr::slice(which.max(as.Date(MMM.YY, '%m/%d/%Y')))
head(test_df)
```

    ## # A tibble: 6 x 21
    ## # Groups:   Emp_ID [6]
    ##   Emp_ID MMM.YY       Age Gender City  Education_Level Salary Dateofjoining
    ##    <int> <date>     <int> <fct>  <fct> <fct>            <int> <date>       
    ## 1      2 2017-12-01    31 Male   C7    Master           67016 2017-11-06   
    ## 2      6 2017-12-01    31 Female C11   Bachelor         78728 2017-07-31   
    ## 3     11 2017-12-01    28 Female C19   Master           42172 2017-12-07   
    ## 4     14 2017-12-01    39 Female C26   College          19734 2017-10-16   
    ## 5     25 2017-12-01    31 Male   C24   Bachelor        102077 2014-10-30   
    ## 6     26 2017-12-01    43 Male   C14   Master          132577 2015-05-07   
    ## # ... with 13 more variables: LastWorkingDate.x <date>,
    ## #   Joining.Designation <int>, Designation <int>, Quarterly.Rating <int>,
    ## #   DaysInCompany <drtn>, lyd <chr>, LastWorkingDate.y <date>,
    ## #   daysb4lastworkingdate <drtn>, attrition <fct>, year <chr>, month <dbl>,
    ## #   day <chr>, Total.Business.Value <int>

``` r
#Remove Test Employee IDs from training set#
#train_data <- train_data[ ! train_data$Emp_ID %in% test$Emp_ID, ]

####partitioning data
df<-train_data %>% filter(MMM.YY < "2017-07-01" )
head(df)
```

    ##   Emp_ID     MMM.YY Age Gender City Education_Level Salary Dateofjoining
    ## 1      1 2016-03-01  28   Male  C23          Master  57387    2015-12-24
    ## 2      1 2016-01-01  28   Male  C23          Master  57387    2015-12-24
    ## 3      1 2016-02-01  28   Male  C23          Master  57387    2015-12-24
    ## 4      4 2017-03-01  43   Male  C13          Master  65603    2016-12-07
    ## 5      4 2017-02-01  43   Male  C13          Master  65603    2016-12-07
    ## 6      4 2017-01-01  43   Male  C13          Master  65603    2016-12-07
    ##   LastWorkingDate.x Joining.Designation Designation Total.Business.Value
    ## 1        2016-03-11                   1           1                    0
    ## 2              <NA>                   1           1              2381060
    ## 3              <NA>                   1           1              -665480
    ## 4              <NA>                   2           2               350000
    ## 5              <NA>                   2           2                    0
    ## 6              <NA>                   2           2                    0
    ##   Quarterly.Rating DaysInCompany lyd LastWorkingDate.y daysb4lastworkingdate
    ## 1                2       68 days Yes        2016-03-11               10 days
    ## 2                2        8 days  No        2016-03-11               70 days
    ## 3                2       39 days  No        2016-03-11               39 days
    ## 4                1       84 days  No        2017-04-27               57 days
    ## 5                1       56 days  No        2017-04-27               85 days
    ## 6                1       25 days  No        2017-04-27              116 days
    ##   attrition year month day
    ## 1       Yes 2016     3  01
    ## 2       Yes 2016     1  01
    ## 3       Yes 2016     2  01
    ## 4       Yes 2017     3  01
    ## 5       Yes 2017     2  01
    ## 6       Yes 2017     1  01

``` r
#plotting attrition data
df %>%
  group_by(attrition) %>%
  tally() %>%
  ggplot(aes(x = attrition, y = n,fill=attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="attrition", y="Count of Attrition")+
  ggtitle("Attrition")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))
```

![](Figs/plot1-1.png)<!-- -->

``` r
## variation of attrition with age
ggplot(data=df, aes(Age))+ geom_histogram(breaks=seq(20, 50, by=2),  col="red", aes(fill=..count..))+labs(x="Age", y="Count")
```

![](Figs/plot2-1.png)<!-- -->

``` r
## variation of attrition by quarterly rating
ggplot(df,aes(x=Quarterly.Rating,fill=attrition),inherit.aes = FALSE)+geom_bar()
```

![](Figs/plot2-2.png)<!-- --> ## variation of attrition w.r.t. education
and income

``` r
avg.income<-df %>% select(Education_Level, Salary, attrition)%>% group_by(Education_Level, attrition) %>% summarize(avg.inc=mean(Salary))%>%
ggplot(aes(x=reorder(Education_Level, avg.inc), y=avg.inc, fill=attrition)) + geom_bar(stat="identity", position="dodge") + facet_wrap(~attrition) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + 
  scale_fill_manual(values=c("lightgreen", "tomato2")) + 
  labs(y="Average Income", x="Education_Level", title="Average Income by Education_Level \n and Attrition Status") + 
  geom_text(aes(x=Education_Level, y=0.01, label= paste0("$ ", round(avg.inc,2))),
            hjust=-0.5, vjust=0, size=3, 
            colour="black", fontface="bold",
            angle=90)
```

    ## `summarise()` has grouped output by 'Education_Level'. You can override using the `.groups` argument.

``` r
avg.income
```

![](Figs/correlogram-1.png)<!-- -->

``` r
options(repr.plot.width=10, repr.plot.height=7) 

nums <- select_if(df, is.numeric)
corr <- round(cor(nums), 1)
ggcorrplot(corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "#01A9DB"), 
           title="Correlogram Employee Attritions", 
           ggtheme=theme_minimal())
```

![](Figs/correlogram-2.png)<!-- --> ## Remove unwanted colum

``` r
## Remove unwanted columns
cols<-c("MMM.YY","LastWorkingDate", "Dateofjoining","LastWorkingDate.x","LastWorkingDate.y","lyd","daysb4lastworkingdate")
df[cols]<-NULL
test_df[cols]<-NULL
```

##%%%%XG BOOST%%

``` r
cols<-c("Salary","Quarterly.Rating","Joining.Designation","Total.Business.Value","Designation","DaysInCompany","Age")
df[cols]<-lapply(df[cols], as.numeric)
test_df[cols]<-lapply(test_df[cols], as.numeric)
df_numeric<-df[cols]
test_df_numeric<-test_df[cols]
```

### One hot Encoding for df

``` r
Gender_numeric <- model.matrix(~Gender-1,df)
City_numeric <- model.matrix(~City-1,df)
Education_level_numeric <- model.matrix(~Education_Level-1,df)
df_numeric<-cbind(df_numeric,Gender_numeric,City_numeric,Education_level_numeric)
df_matrix<-data.matrix(df_numeric)
```

``` r
#onehot encoding for test
Gender_numeric <- model.matrix(~Gender-1,test_df)
City_numeric <- model.matrix(~City-1,test_df)
Education_level_numeric <- model.matrix(~Education_Level-1,test_df)
test_df_numeric<-cbind(test_df_numeric,Gender_numeric,City_numeric,Education_level_numeric)
test_df_matrix<-data.matrix(test_df_numeric)


df$attrition <- ifelse(df$attrition == "Yes", 1, 0)
test_df$attrition<-ifelse(test_df$attrition == "Yes", 1, 0)

#labels
train_dat<-df_matrix
test_dat<- test_df_matrix
train_labels<-df$attrition
test_labels<-test_df$attrition
```

#converting to Dmatrix

``` r
dtrain <- xgb.DMatrix(data = train_dat, label= train_labels)
dtest <- xgb.DMatrix(data = test_dat, label= test_labels)

model <- xgboost(data = dtrain, # the data   
                 nround = 20, # max number of boosting iterations
                 objective = "binary:logistic")
```

    ## [15:03:00] WARNING: amalgamation/../src/learner.cc:1115: Starting in XGBoost 1.3.0, the default evaluation metric used with the objective 'binary:logistic' was changed from 'error' to 'logloss'. Explicitly set eval_metric if you'd like to restore the old behavior.
    ## [1]  train-logloss:0.594290 
    ## [2]  train-logloss:0.538056 
    ## [3]  train-logloss:0.503478 
    ## [4]  train-logloss:0.480051 
    ## [5]  train-logloss:0.463478 
    ## [6]  train-logloss:0.451242 
    ## [7]  train-logloss:0.442515 
    ## [8]  train-logloss:0.432905 
    ## [9]  train-logloss:0.425032 
    ## [10] train-logloss:0.419400 
    ## [11] train-logloss:0.414647 
    ## [12] train-logloss:0.409879 
    ## [13] train-logloss:0.404354 
    ## [14] train-logloss:0.400843 
    ## [15] train-logloss:0.397112 
    ## [16] train-logloss:0.394142 
    ## [17] train-logloss:0.387689 
    ## [18] train-logloss:0.383489 
    ## [19] train-logloss:0.376327 
    ## [20] train-logloss:0.371369

``` r
pred <- predict(model, dtest)
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))
```

    ## [1] "test-error= 0.365721997300945"

``` r
predictions<- (as.numeric(pred > 0.5) != test_labels)
predictions<-as.integer(predictions)
```

#confusion matrix

``` r
conf_df <- data.frame(table(test_df$attrition, predictions))
ggplot(data =  conf_df, mapping = aes(x = predictions, y = Var1)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "#F3F781", high = "#58FA82") +
  theme_economist() + theme(legend.position="none", strip.background = element_blank(), strip.text.x = element_blank(), 
                            plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),
                            axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                    
                                    axis.title=element_text(colour="white"), 
                            legend.background = element_rect(fill="#FFF9F5",
                                                             size=0.5, linetype="solid", 
                                                             colour ="black")) + 
  labs(title="Confusion Matrix", y="Attrition Status", x="Predictions")
```

![](Figs/cnfmatrix-1.png)<!-- -->

``` r
test$Target<-predictions

write.csv(test,file="My _pediction.csv", row.names = FALSE)
```

The test cases were chosen such that all of them were belonging to ‘0’
label
