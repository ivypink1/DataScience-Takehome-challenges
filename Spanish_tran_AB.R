# objective: 1). confirm the test is actually negative; 2) what happened, is localication really worse;
#3). design an algorithm to catch issue when similar things happened 


library(dplyr)
library(rpart)
library(ggplot2)
library(rpart.plot)

setwd("/Users/sma76/Desktop/data science/R/take-home challenges/AB test/")
t <-read.csv('test_table.csv', header =TRUE)
u <-read.csv('user_table.csv', header=TRUE)

names(t)
names(u)

summary(t)
summary(u)

## join the two tables by user_id
length(unique(t$user_id)) == length(t$user_id)
length(unique(u$user_id)) == length(u$user_id)

#?ensure that we don't lose user_id in test but not in user table
length(unique(u$user_id)) -length(unique(t$user_id)) 


data <-merge(x=t, y =u, by='user_id', all.x=TRUE)
data$date =as.Date(data$date)

#check to see if Spain converts much better than the rest of LatAm countries
data_conversion_country <- data %>%
        group_by(country) %>%
        summarize( conversion =mean(conversion[test==0])) %>%
        arrange (desc(conversion))

head(data_conversion_country)

# use random forest to see which variable(s) play important role in conversion

# simple t-test
data_test =subset(data, country !='Spain')
t.test(data_test$conversion[data_test$test ==1], data_test$conversion[data_test$test ==0])

data_conversion_day =data_test %>%
        group_by(date) %>%
        summarize(test_vs_control =mean(conversion[test==1])/
                          mean(conversion[test==0]))

qplot(date, test_vs_control, data= data_conversion_day, geom="line", group =1)

tree =rpart(test ~ ., data_test[,-8],
            control= rpart.control(minbucket = nrow(data_test)/100, 
                                   maxdepth = 2))

data_test_country =data_test %>%
        group_by(country) %>%
        summarize( p-vale =t.test( conversion[test ==1], conversion
                                   [test==0]) $p.value,
                   conversion_test =t.test( conversion[test==1], conversion[test==0]$estimate[1],
                                            conversion_control =t.test( conversion[test==1], conversion[test==0]$estimate[2])
                                            %>%
                arrange(p_value)
