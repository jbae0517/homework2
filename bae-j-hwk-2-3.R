#Jennifer Bae
#Homework 2-3


knitr::opts_chunk$set(echo = TRUE)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales,
               kableExtra)

#reading the data and saving as variables
HCRIS_Data_v1996 <- readRDS("/Users/jenniferbae/Downloads/ECON 470 R Coding/homework2/data/output/HCRIS_Data_v1996.rds")
HCRIS_Data_v2010 <- readRDS("/Users/jenniferbae/Downloads/ECON 470 R Coding/homework2/data/output/HCRIS_Data_v2010.rds")
final.hcris.data <- readRDS("/Users/jenniferbae/Downloads/ECON 470 R Coding/homework2/data/output/HCRIS_Data.rds")


# 1. How many hospitals filed more than one report in the same year? 


#create missing variables for columns introduced in v2010 of hcris forms
HCRIS_Data_v1996 = HCRIS_Data_v1996 %>%
  mutate(hvbp_payment=NA, hrrp_payment=NA)

#combining v1996 and v2010 hcris forms
#sorting by provider number and year

final.hcris=rbind(HCRIS_Data_v1996,HCRIS_Data_v2010) %>%
  mutate(fy_end=mdy(fy_end),fy_start=mdy(fy_start),
         date_processed=mdy(date_processed),date_created=mdy(date_created),
         tot_discounts=abs(tot_discounts), hrrp_payment=abs(hrrp_payment)) %>%
  mutate(fyear=year(fy_end)) %>%
  arrange(provider_number,fyear) %>%
  dplyr::select(-year)

#creating number of reports, grouped by hospital fiscal year 
final.hcris =
  final.hcris %>% 
  add_count(provider_number, fyear, name="total_reports")

dup.report.data <- final.hcris %>% filter(total_reports > 1) %>%
  group_by(fyear) %>% summarize(count = length(unique(provider_number)))

dup.report <- dup.report.data %>%
  ggplot(aes(x = fyear, y = count)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = c(1997:2017)) +
  labs(x = "Year", y = "Number of Hospitals", Title = "Number of Hospitals with more than 1 Report in each Year from 1997 to 2018") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

dup.report



# 2. After removing/combining multiple reports,how many unique hospital IDs (Medicare provider numbers) exist in the data?

unique_provider_num <- n_distinct(final.hcris.data$provider_number)
unique_provider_num


# 3. What is the distribution of total charges (tot_charges in the data) in each year? 
#Show your results with a “violin” plot, with charges on the y-axis and years on the x-axis.

dist.tot <- final.hcris.data %>% group_by(year) %>%
  summarise(
    Mean = mean(tot_charges, na.rm = TRUE),
    Min = min(tot_charges, na.rm = TRUE),
    Q1 = quantile(tot_charges, 0.25, na.rm = TRUE),
    Median = median(tot_charges, na.rm = TRUE),
    Q3 = quantile(tot_charges, 0.75, na.rm= TRUE),
    Max = max(tot_charges, na.rm = TRUE)
  ) %>%
  mutate(year = factor(year))

fig.tot.charges <- final.hcris.data %>%
  ggplot(aes(x = factor(year), y = tot_charges)) +
  geom_violin(alpha = 0.9, draw_quantiles = c(0.5)) + 
  labs(x= "Year", y = "Total Charges", Title = "Distribution of Total Charges in Each Year from 1997 to 2018") +
  theme_classic() + 
  theme(
    plot.title = element_text(size=12, color = "black", hjust=0.5),
    axis.title = element_text (size=10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

fig.tot.charges

# 4. What is the distribution of estimated prices in each year? 
#Again present your results with a violin plot, and recall our formula for estimating prices from class.

price.estimation <- final.hcris.data %>%
  mutate(discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom) 

dist.est <- price.estimation %>% group_by(year) %>%
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Min = min(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm= TRUE),
    Max = max(price, na.rm = TRUE)
  ) %>%
    mutate(year= factor(year))
  
fig.est.charges <- price.estimation %>%
  ggplot(aes(x = factor(year), y = price)) +
  geom_violin(alpha = 1.0, draw_quantiles = c(0.5)) + 
  
  labs(x= "Year", y = "Estimated Prices", Title = "Distribution of Estimated Prices in Each Year from 1997 to 2018") +
  theme_classic() + 
  theme(
    plot.title = element_text(size=12, color = "black", hjust=0.5),
    axis.title = element_text (size=10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

fig.est.charges 

#Estimate ATEs
# 5. Calculate the average price among penalized versus non-penalized hospitals.

final.hcris <- price.estimation %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>% 
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)),
          penalty = (hvbp_payment-hrrp_payment<0)) 

mean.pen <- round(mean(final.hcris$price[which(final.hcris$penalty==1)]),2)
mean.nopen <- round(mean(final.hcris$price[which(final.hcris$penalty==0)]),2)


# 6. Split hospitals into quartiles based on bed size

final.hcris$quartile <- ntile(final.hcris$beds, 4)

final.hcris$quartile_1 <- ifelse(final.hcris$quartile == 1, 1,0)
final.hcris$quartile_2 <- ifelse(final.hcris$quartile == 2, 1,0)
final.hcris$quartile_3 <- ifelse(final.hcris$quartile == 3, 1,0)
final.hcris$quartile_4 <- ifelse(final.hcris$quartile == 4, 1,0)

quartile.data <- final.hcris %>% group_by(quartile, penalty) %>% summarise(mean(price))
quartile.data$quartile <- factor(quartile.data$quartile)
quartile.data$penalty <- factor(quartile.data$penalty)
levels(quartile.data$quartile) <- c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile")
levels(quartile.data$penalty) <- c("Non-Penalized", "Penalized")

quartile.data <- quartile.data %>% pivot_wider(names_from = penalty, values_from = `mean(price)`)
quartile.data


# 7. Find the average treatment effect using each of the following estimators


# nearest neighbor inverse 
library(dplyr)

lp.vars <- final.hcris %>% 
  dplyr::select(price, penalty, quartile_1, quartile_2, quartile_3) 

lp.covs <- lp.vars %>% dplyr::select(quartile_1, quartile_2, quartile_3)

m.nn.inverse <- Matching::Match(Y=lp.vars$price,
                                Tr=lp.vars$penalty,
                                X=lp.covs,
                                M=1,
                                Weight=1,
                                estimand="ATE")
summary(m.nn.inverse)


# nearest neighbor mahalanobis
m.nn.mahalanobis <- Matching::Match(Y=lp.vars$price,
                                    Tr=lp.vars$penalty,
                                    X=lp.covs,
                                    M=1,
                                    Weight=2,
                                    estimand="ATE")
summary(m.nn.mahalanobis)
m.nn.mahalanobis$est


# propensity score weighting 
logit.model <- glm(penalty ~ final.hcris$quartile_1 + final.hcris$quartile_2 +final.hcris$quartile_3, data=lp.vars)
ps <- fitted(logit.model)


m.nn.propensity <- Matching::Match(Y=lp.vars$price,
                                   Tr=lp.vars$penalty,
                                   X=ps,
                                   M=1,
                                   estimand="ATE")

summary(m.nn.propensity)
m.nn.propensity$est



# inverse propensity score weighting
lp.vars <- lp.vars %>%
  mutate(ipw = case_when(
    penalty==1 ~ 1/ps,
    penalty==0 ~ 1/(1-ps),
    TRUE ~ NA_real_
  ))    

mean.t1 <- lp.vars %>% filter(penalty==1) %>%
  select(price, ipw) %>% 
  summarize(mean_p=weighted.mean(price,w=ipw))

mean.t0 <- lp.vars %>% filter(penalty==0) %>%
  select(price, ipw) %>% 
  summarize(mean_p=weighted.mean(price,w=ipw))

mean.t1$mean_p - mean.t0$mean_p
ipw.reg <- lm(price ~ penalty, data=lp.vars, weights=ipw)



summary(ipw.reg)
inverse_propen <- summary(ipw.reg)$coefficients[2,1]


# linear regression
reg.dat <- lp.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
  
  mutate(quartile_1_diff = penalty*(quartile_1 - mean(quartile_1)),
         quartile_2_diff = penalty*(quartile_2 - mean(quartile_2)),
         quartile_3_diff = penalty*(quartile_3 - mean(quartile_3)))

reg <- lm(price ~ penalty + quartile_1 + quartile_2 + quartile_3 + quartile_1_diff + quartile_2_diff + quartile_3_diff,
          data=reg.dat)


summary(reg)
linear_reg <-summary(reg)$coefficients[2,1]


# creating the table to show all ATE
ATE_table <- data.frame(Estimators = c("NN Matching, inverse variance", "NN Matching, mahalanobis",
                                     "Inverse pscore weighting", "Linear Regression"),
                      ATE = c(m.nn.inverse$est, m.nn.mahalanobis$est, inverse_propen, linear_reg ))







# 8. With these different treatment effect estimators, are the results similar, identical, very different?

# 9. Do you think you’ve estimated a causal effect of the penalty? Why or why not? (just a couple of sentences)

# 10. Briefly describe your experience working with these data (just a few sentences). Tell me one thing you learned and one thing that really aggravated you.


save.image("Hwk2_workspace.Rdata")
