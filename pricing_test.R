# Analyzing AB test results, to see which metrics are more critical for the company, and run power analysis 
# to determine the minimum sample size to reach to significance

library(dplyr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pwr)
library(lubridate)
library(rattle)

setwd("/Users/sma76/Desktop/data science/R/take-home challenges/pricing_test/")

u <-read.csv("Pricing_Test/user_table.csv", header=TRUE)
t <-read.csv("Pricing_Test/test_results.csv", header=TRUE)

str(u)
str(t)

user_diff <- length(unique(u$user_id))-length(unique(t$user_id))

# the num of unique user_id in test is more than that in user table, thus left_join
d <- left_join(t, u, by="user_id")

summary(d) #this users in this test are mainly/almost in the USA, thus geographtical location interest will remain in diff cities/lat/long

# to get actionable advice, to investigate the difference among different categorical variables

d_conversion_source <- d %>%
        group_by(source) %>%
        summarize(conversion =mean(converted)) %>%
        arrange(desc(conversion))

g1 <-ggplot(d_conversion_source, aes(source, conversion))
g1+geom_bar(stat='identity')

d_conversion_device <- d %>%
        group_by(device) %>%
        summarize(conversion =mean(converted)) %>%
        arrange(desc(conversion))
g2 <-ggplot(d_conversion_device, aes(device, conversion))
g2+geom_bar(stat='identity')

d_conversion_os <- d %>%
        group_by(operative_system) %>%
        summarize(conversion =mean(converted)) %>%
        arrange(desc(conversion))
g3 <-ggplot(d_conversion_os, aes(operative_system, conversion))
g3+geom_bar(stat='identity')

d_conversion_city <- filter(d, country == "USA") %>%
        group_by(city) %>%
        summarize(conversion =mean(converted)) %>%
        arrange(desc(conversion))

g4 <-ggplot(d_conversion_city, aes(city, conversion))
g4+geom_bar(stat='identity')

d_conversion <- d %>%
        group_by(test) %>%
        summarize(conversion =mean(converted)) %>%
        arrange(desc(conversion))



#simple t_test (how to handle unequal number of sample in control and experiment)
s1 <-sample(202727, 114073)
control_valid <- filter(d, test ==0) 
control_valid <-control_valid[s1,]

  
t_test_all <-t.test(control_valid$converted, d$converted[d$test==1])
t_test_all

tree <-rpart(converted ~ source+operative_system+test, d)

d_test_city <-filter(d, country=='USA') %>%
        group_by(city) %>%
        summarize(p-value = t.test(d[test==1], d[test==0])$p.value, conversion_test =t.test(d[test==1], d[test==0]$estimate[1],conversion_control =t.test(d[test==1], d[test==0]$estimate[2])))
        

# rpart 
s <-sample(316800,300000)
training <-d[s,]
test <-d[-s,]

tree_1 <-rpart(converted ~ source+operative_system+test, training, method ='class' )

p <-predict (tree_1, test, type ="class")
table(test[,16800], p)


#power analysis
daily_traffic <- filter(d, test==1) %>% nrow()/unique(ymd(d[,2]))

#effect size: https://www.uccs.edu/~lbecker/
mean <- mean(d$converted)

data <- filter(d, test == 1) %>% select (converted)
sd(data)

pool_sd <- sqrt(sd(filter(d, test==0))^2) + sd(filter(d, test==1))^2
mse <- mean((d$converted - mean)^2)
d_effect <- abs(0.01996090-0.01554268)/mse

n <-pwr.t.test (d=8.894, sig.level = 0.05, power =0.8, type =c("two.sample"))




