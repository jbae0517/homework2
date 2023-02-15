#Jennifer Bae
#Homework 2-2

knitr::opts_chunk$set(echo = TRUE)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales,
               kableExtra)

HCRIS_Data_v1996 <- readRDS("/Users/jenniferbae/Downloads/ECON 470 R Coding/homework2/data/output/HCRIS_Data_v1996.rds")
HCRIS_Data_v2010 <- readRDS("/Users/jenniferbae/Downloads/ECON 470 R Coding/homework2/data/output/HCRIS_Data_v2010.rds")
final.hcris.data <- readRDS("/Users/jenniferbae/Downloads/ECON 470 R Coding/homework2/data/output/HCRIS_Data.rds")


# 1. How many hospitals filed more than one report in the same year? 
#Show your answer as a line graph of the number of hospitals over time.

reports_per_year <- final.hcris.data %>%
  group_by(street,year) %>%
  summarize(n=n())
multiple_reports_per_year <- reports_per_year %>% filter(n>1)

#n_multiple_reports_per_year <- n_distinct(multiple_reports_per_year$street)

filtered_data <- multiple_reports_per_year %>%
  group_by(year) %>%
  summarize(count=n())
sum(filtered_data$count)

fig.multiple.reports <- ggplot(filtered_data,aes(year,count)) + geom_line() + labs(title="Number of hospitals that filed more than one report in the same year", x = "Year", y= "Number of hospitals") + theme_bw()
fig.multiple.reports

# 2. After removing/combining multiple reports, 
#how many unique hospital IDs (Medicare provider numbers) exist in the data?

unique_provider_num <- n_distinct(final.hcris.data$provider_number)
unique_provider_num


# 3. What is the distribution of total charges (tot_charges in the data) in each year? 
#Show your results with a “violin” plot, with charges on the y-axis and years on the x-axis.


fig.tot.charges <- ggplot(final.hcris.data, aes(x=as.factor(year), y=tot_charges, fill="")) + geom_violin() +
  scale_y_continuous(trans = "log10") + ggtitle("Distribution of Total Charges") + xlab("Year") + ylab("Total Charges scaled by log10") + theme(plot.title = element_text(hjust = 0.5))
fig.tot.charges


# 4. What is the distribution of estimated prices in each year? 
#Again present your results with a violin plot, and recall our formula for estimating prices from class.

price.estimation <- final.hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          est_price = price_num/price_denom)

#Violin Plot of Prices over Time
ggplot(price.estimation, aes(x = as.character(year), y=est_price, )) + 
  geom_violin(fill="light blue") + scale_y_continuous(trans = "log10") + ggtitle("Distribution of Prices") + xlab("Year") + ylab("Price scaled by log10") + theme(plot.title = element_text(hjust = 0.5))

#Estimate ATEs
# 5. Calculate the average price among penalized versus non-penalized hospitals.

final.hcris <- price.estimation %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         est_price<100000, 
         beds>30, year==2012) %>% 
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)),
          penalty = (hvbp_payment-hrrp_payment<0)) 

mean.pen <- round(mean(final.hcris$est_price[which(final.hcris$penalty==1)]),2)
mean.nopen <- round(mean(final.hcris$est_price[which(final.hcris$penalty==0)]),2)
mean.pen
mean.nopen

# 6. Split hospitals into quartiles based on bed size


final.hcris$quartile <- ntile(final.hcris$beds, 4)

final.hcris$quartile_1 <- ifelse(final.hcris$quartile == 1, 1,0)
final.hcris$quartile_2 <- ifelse(final.hcris$quartile == 2, 1,0)
final.hcris$quartile_3 <- ifelse(final.hcris$quartile == 3, 1,0)
final.hcris$quartile_4 <- ifelse(final.hcris$quartile == 4, 1,0)

fig_6 <- final.hcris %>% filter(!is.na(penalty)) %>%
  group_by(quartile, penalty) %>%
  summarise(avg_price = mean(est_price, na.rm = TRUE))

fig_6


# 7. Find the average treatment effect using each of the following estimators, and present your results in a single table


#nearest neighbor inverse 
library(dplyr)
lp.vars <- final.hcris$quartile %>% 
  select(beds, mcaid_discharges, penalty, ip_charges, 
         mcare_discharges, tot_mcare_payment, est_price, quartile_1, quartile_2, quartile_3, quartile_4, quartile) %>%
  filter(complete.cases(.))
lp.covs <- lp.vars %>% select(-c("quartile","est_price"))

m.nn.var <- Matching::Match(Y=lp.vars$est_price,
                            Tr=lp.vars$final.hcris$quartile,
                            X=lp.covs,
                            M=4,
                            Weight=1,
                            estimand="ATE")
summary(m.nn.var)


#nearest neighbor mahalanobis

m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")

#inverse propensity score weighting 
logit.model <- glm(penalty ~ beds + mcaid_discharges + ip_charges + mcare_discharges +
                     tot_mcare_payment + final.hcris$quartile_1 + final.hcris$quartile_2 +final.hcris$quartile_3 + final.hcris$quartile_4, family=binomial, data=lp.vars)
ps <- fitted(logit.model)
lp.vars <- lp.vars %>%
  mutate(ipw = case_when(
    penalty==1 ~ 1/ps,
    penalty==0 ~ 1/(1-ps),
    TRUE ~ NA_real_
  ))
mean.t1 <- lp.vars %>% filter(penalty==1) %>%
  select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
mean.t0 <- lp.vars %>% filter(penalty==0) %>%
  select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
mean.t1$mean_p - mean.t0$mean_p
ipw.reg <- lm(price ~ penalty, data=lp.vars, weights=ipw)
summary(ipw.reg)



#linear regression
reg.dat <- lp.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
  mutate(beds_diff = penalty*(beds - mean(beds)),
         mcaid_diff = penalty*(mcaid_discharges - mean(mcaid_discharges)),
         ip_diff = penalty*(ip_charges - mean(ip_charges)),
         mcare_diff = penalty*(mcare_discharges - mean(mcare_discharges)),
         mpay_diff = penalty*(tot_mcare_payment - mean(tot_mcare_payment)))
reg <- lm(price ~ penalty + beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment + 
            beds_diff + mcaid_diff + ip_diff + mcare_diff + mpay_diff,
          data=reg.dat)
summary(reg)


# 8. With these different treatment effect estimators, are the results similar, identical, very different?

# 9. Do you think you’ve estimated a causal effect of the penalty? Why or why not? (just a couple of sentences)

# 10. Briefly describe your experience working with these data (just a few sentences). Tell me one thing you learned and one thing that really aggravated you.
