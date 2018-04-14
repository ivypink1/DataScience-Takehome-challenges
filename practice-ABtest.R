library(dplyr)
library(data.table)
library(graphics)
library(pwr)


setwd("/Users/sma76/Desktop/AB testing/ce_v7/")
d <- fread("cev7_abtest_data", sep="\t", header=TRUE)

names(d) <-c("test_id","user_id", "visit_id", "query", "date_id", "time_stamp", "clicked", "added", "converted", "revenue", "url_attr_clicked", "url_attr_pv", "segment_json")

dim(d)
str(d)
summary(d)

# define control and feature test_id
control = '6WBcp.-USSp'
feature = '6WBcp.5bMPc'


#data clearning and manipulation
test_duration <-length(unique(d$date_id))
test_fitler <-unique(d$test_id)
traffic_control <- filter(d, test_id == control) %>% nrow()
traffic_feature <- filter(d, test_id == feature) %>% nrow()

# 1.sanity check on invariant: traffic for control and feature/proportional test
traffic_control-traffic_feature #traffic is different for control and feature, thus we need to do hypethesis test whether the traffic allocation is probably

## the assumption is that we allocate the traffice between control and feature as 50/50, thus the H0: Pcontrol = 0.5

P_control <- traffic_control/(traffic_control+traffic_feature)
SE <- sqrt(0.5*0.5/(traffic_control+traffic_feature))
CI_95_Pcontrol_upper <- 0.5+1.96*SE
CI_95_Pcontrol_lower <-0.5-1.96*SE

total_traffic = traffic_control+traffic_feature

## the P_control is outside of the CI_95, control and feature are not equally assigned with traffic
# the way to treat unequal sample size is to do propentisty score matching by running glm: logistic regression
(https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html)


#2. aggregate the metrics by query in control/feature and computing metrics for analysis (CTR, ATC, conversion, and revenue)

d_traffic <-d %>%
        group_by(query, test_id) %>%
        summarise(traffic = n())

d_query <- d %>% 
        group_by(query, test_id) %>%
        summarise(total_click =sum(clicked))

d_cart <-d %>% 
        group_by(query, test_id) %>%
        summarise(total_cart =sum(added))
d_conv <-d %>% 
        group_by(query, test_id) %>%
        summarise(total_conv =sum(converted))

d_rev <-d %>% 
        group_by(query, test_id) %>%
        summarise(total_rev =sum(revenue))

d_analyze <- cbind(d_traffic, d_query, d_cart, d_conv, d_rev) %>%
        select(query, test_id, traffic, total_click, total_cart, total_conv, total_rev)

d_analyze_metric<- d_analyze %>%
        mutate(ctr =total_click/traffic) %>%
        mutate(atc = total_cart/total_click) %>%
        mutate(con = total_conv/total_cart)

# replace NaN as 0
data_tmp <-d_analyze_metric
data_tmp$ctr[is.na(data_tmp$ctr)] <-0
data_tmp$atc[is.na(data_tmp$atc)] <-0
data_tmp$con[is.na(data_tmp$con)] <-0


# 3.1 Null hypotheis testing
t1 <- t.test(filter(data_tmp, test_id == control)$ctr, filter(data_tmp, test_id == feature)$ctr)
t2 <- t.test(filter(data_tmp, test_id == control)$atc, filter(data_tmp, test_id == feature)$atc)
t3 <- t.test(filter(data_tmp, test_id == control)$con, filter(data_tmp, test_id == feature)$con)
t4 <- t.test(filter(data_tmp, test_id == control)$total_rev, filter(data_tmp, test_id == feature)$total_rev)


# 3.2 bayesian A/B testing
# 3.3 sample size determination

d = abs(u1-u2)/sqrt(common_error_variance)

pwr.t.test(d=0.1, sig.level= 0.05, power = 0.8, type ="two.sample")


# 4. outlier detection: (https://www.r-bloggers.com/outlier-detection-and-treatment-with-r/)

# 5.deep dive and exploratory with graph





