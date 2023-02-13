
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


hcris_2012 <- price.estimation %>% filter(year == 2012)

hcris_2012$penalty <- ifelse(hcris_2012$hrrp_payment + hcris_2012$hvbp_payment < 0, 1,0)

pen_price<- hcris_2012 %>%
  filter(!is.na(penalty)) %>% 
  filter(penalty == 1) %>%
  group_by(penalty) %>% 
  summarise(price = mean(est_price, na.rm = TRUE))

non_pen_price <- hcris_2012 %>%
  filter(!is.na(penalty)) %>% 
  filter(penalty == 0) %>%
  group_by(penalty) %>% 
  summarise(price = mean(est_price, na.rm = TRUE))

pen_price
non_pen_price

# 6. Split hospitals into quartiles based on bed size


hcris_2012$quartile <- ntile(hcris_2012$beds, 4)

hcris_2012$quartile_1 <- ifelse(hcris_2012$quartile == 1, 1,0)
hcris_2012$quartile_2 <- ifelse(hcris_2012$quartile == 2, 1,0)
hcris_2012$quartile_3 <- ifelse(hcris_2012$quartile == 3, 1,0)
hcris_2012$quartile_4 <- ifelse(hcris_2012$quartile == 4, 1,0)

fig_6 <- hcris_2012 %>% filter(!is.na(penalty)) %>%
  group_by(quartile, penalty) %>%
  summarise(avg_price = mean(est_price, na.rm = TRUE))

fig_6

# 7. Find the average treatment effect using each of the following estimators, and present your results in a single table

lp.vars <- hcris_2012$quartile %>% 
  select(beds, mcaid_discharges, penalty, ip_charges, 
         mcare_discharges, tot_mcare_payment, price, hcris_2012$quartile_1, hcris_2012$quartile_2, hcris_2012$quartile_3, hcris_2012$quartile_4, hcris_2012$quartile) %>%
  filter(complete.cases(.))
lp.covs <- lp.vars %>% select(-c( "bed.quantile","price"))
m.nn.var <- Matching::Match(Y=lp.vars$price,
                            Tr=lp.vars$hcris_2012$quartile_1,
                            X=lp.covs,
                            M=4,
                            Weight=1,
                            estimand="ATE")
summary(m.nn.var)



# 8. With these different treatment effect estimators, are the results similar, identical, very different?

# 9. Do you think you’ve estimated a causal effect of the penalty? Why or why not? (just a couple of sentences)

# 10. Briefly describe your experience working with these data (just a few sentences). Tell me one thing you learned and one thing that really aggravated you.
