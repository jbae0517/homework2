
#Jennifer Bae
#Homework 2-1

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

price.estimate <- final.hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom)

#Violin Plot of Prices over Time
ggplot(price.estimate, aes(x = as.character(year), y=price, )) + 
  geom_violin(fill="light blue") + scale_y_continuous(trans = "log10") + ggtitle("Distribution of Prices") + xlab("Year") + ylab("Price scaled by log10") + theme(plot.title = element_text(hjust = 0.5))

#Estimate ATEs
# 5. Calculate the average price among penalized versus non-penalized hospitals.

final.hcris <- price.estimate %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>% 
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)),
          penalty = (hvbp_payment-hrrp_payment<0)) 
mean.pen <- round(mean(final.hcris$price[which(final.hcris$penalty==1)]),2)
mean.nopen <- round(mean(final.hcris$price[which(final.hcris$penalty==0)]),2)
mean.pen
mean.nopen