# identify fraudulent activity: supervise learning

library(dplyr)
library(randomForest)
library(ROCR)
library(rpart)
library(ggplot2)

setwd("/Users/sma76/Desktop/data science/R/take-home challenges/fraud/Fraud/")
data <- read.csv("Fraud_Data.csv", header = TRUE)
ip_addresses <- read.csv("IpAddress_to_Country.csv", header = TRUE)

dim(data)
dim(ip)

summary(data)
summary(ip)

# determine where each user comes from
data_country = rep(NA, nrow(data))
for (i in 1: nrow(data)){
        tmp = as.character(ip_addresses [data$ip_address[i] >= ip_addresses$lower_bound_ip_address & data$ip_address[i] <= ip_addresses$upper_bound_ip_address,
                                         "country"])
        if (length(tmp) == 1) {data_country[i] = tmp}
}
data$country = data_country


data[, "signup_time"] <-as.POSIXct(data[,"signup_time"], tz="GMT")
data[, "purchase_time"] <-as.POSIXct(data[,"purchase_time"], tz="GMT")

summary(as.factor(data$country))

#feature engineering: what could be the important variables 
## 1. explortary of exisiting variables
length(unique(data$user_id)) 
nrow(data) # for this dataset, all the user_id only appear once
length(unique(data$device_id)) # this indicates that there are more than 1 users using the same device
length(unique(data$ip_address)) # this indicates that there are more than 1 users are sharing the same ip address

## 2.feature engineering based on explotory analysis
data <- data %>%
        group_by(device_id) %>%
        mutate(device_id_count =n())
data <- data %>%
        group_by(ip_address) %>%
        mutate(ip_address_count =n())

data$purchase_signup_diff =as.numeric(difftime(as.POSIXct(data$purchase_time, tz="GMT"), as.POSIXct(data$signup_time, tz="GMT"), unit="secs"))


data$signup_time_wd = format(data$signup_time, "%A")
data$signup_time_wd = format(data$signup_time, "%A")


# day of the week
data$signup_time_wd = format(data$signup_time, "%A")
# week of the year
data$signup_time_wy =as.numeric(format(data$signup_time, "%U"))

# data set for the model. Drop first 3 vars and device id
data_rf <- data[,-c(1:3, 5)]

#replace the NA in the country var
data_rf$country[is.na(data_rf$country)] ="Not_found"
#just keep the top 50 country, everthing else is "other"
data_rf$country =ifelse(data_rf$country %in% names(sort(table(data_rf$country), descreasing =TRUE)) [51:length(unique(data_rf$country))],  "other", as.character(data_rf$country))
## make "class" a factor
data_rf$class =as.factor(data_rf$class)

## all characters become factors
data_rf[sapply(data_rf, is.character)] <-lapply(data_rf[sapply(data_rf, is.character)], as.factor)

## build randomforestry 
# train/test split
train_sample =sample(nrow(data_rf), size =nrow(data)*0.66)
train_data = data_rf[train_sample,]
test_data = data_rf[-train_sample,]


# model
rf = randomForest(y=train_data$class, x= train_data[, -7], 
                  ytest=test_data$class, xtest =test_data[, -7], 
                  ntree =50, mtry =3, keep.forest = TRUE)


## use ROC (Receive Operating Characteristic)
rf_results = data.frame(true_values = test_data$class,
                        predictions =rf$test$votes[,2])

identical( as.numeric(as.character(rf$test$predicted)), ifelse (rf_results$predictions>0.5, 1, 0))

### didn't work 
pred = predict(rf_results$predictions, rf_results$true_values)
perf = performance (pred, measure = 'tpr', x.measure = "fpr")
plot(perf) + abline(a=0, b=1, col = 'red')
