##### VARIABLES 2008 #####

rm(list = ls())
library(stargazer)
library(Hmisc)
library(tidyverse)
library(haven)
library(zoo)
library(dplyr)

# Set working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/PhD/Accessibility and Labor Market Outcomes")


# Control variables --------------------------------------------------------------------

# Individual and household characteristics

data_2008 <- as_tibble(read_dta("2008_Q1a_New.dta", encoding="VISCII")) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, p2stt_, #individual identity variables
               p2q2_, p2q3_, p2q4_, p3q8_, p3q13_, p3q14_, p3q16_ # individual characteristics
               ) %>% 
        rename(head_relationship = p2q2_ ,
               gender = p2q3_,
               year_born = p2q4_,
               married = p3q8_,
               attending_school = p3q13_,
               years_of_education = p3q14_,
               diplome = p3q16_,
               hhid = p2stt_)

data_2008 <- data_2008 %>% group_by(tinh_2008, quan_2008, xa_2008, ma_h0_2008) %>% 
        mutate(hhsize = max(p2stt_)) # household size

data_2008 <- as_tibble(read_dta("Phieu_1_New_08.dta", encoding = "VISCII")) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, p34q10, p34q4) %>% 
        rename(income = p34q10) %>% # income
        mutate(hhenterprise = ifelse(p34q4 != 0, 1, p34q4)) %>%  # household enterprise
        select(-p34q4) %>% 
        right_join(data_2008, by = c("tinh_2008", "quan_2008", "xa_2008", "ma_h0_2008"))

data_2008 <- as_tibble(read_dta("Q8_New_08.dta", encoding = "VISCII")) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, p43q7_) %>% 
        mutate(loan = ifelse(p43q7_ == 99, 0, p43q7_)) %>%
        group_by(tinh_2008, quan_2008, xa_2008, ma_h0_2008) %>% 
        mutate(loan = sum(loan)) %>% # loan
        select(-p43q7_) %>% 
        distinct() %>% 
        right_join(data_2008, by = c("tinh_2008", "quan_2008", "xa_2008", "ma_h0_2008")) %>% 
        mutate(loan = ifelse(is.na(loan) == TRUE, 0, loan)) # Loan

data_2008 <- as_tibble(read_dta("Q2_New_08.dta", encoding = "VISCII")) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, p6q3_) %>% 
        mutate(landarea = sum(p6q3_, na.rm = T)) %>% # Land size
        select(-p6q3_) %>% 
        distinct() %>% 
        right_join(data_2008, by = c("tinh_2008", "quan_2008", "xa_2008", "ma_h0_2008"))
        
data_2008 <- as_tibble(read_dta("Q2a_New_08.dta", encoding = "VISCII")) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, p9q2_) %>%
        rename(redbook = p9q2_) %>% 
        group_by(tinh_2008, quan_2008, xa_2008, ma_h0_2008) %>% 
        mutate(redbook = min(redbook)) %>% # Redbook
        distinct() %>% 
        right_join(data_2008, by = c("tinh_2008", "quan_2008", "xa_2008", "ma_h0_2008"))

data_2008 <- as_tibble(read_dta("Q7d_New_08.dta", encoding = "VISCII")) %>%
        mutate(price = p42q1_*p42q3_) %>% 
        group_by(tinh_2008, quan_2008, xa_2008, ma_h0_2008) %>% 
        mutate(durable = sum(price)) %>% # Durables
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, durable) %>% 
        distinct() %>% 
        right_join(data_2008, by = c("tinh_2008", "quan_2008", "xa_2008", "ma_h0_2008"))

data_2008 <- as_tibble(read_dta("Q9b_New_08.dta", encoding = "VISCII")) %>%
        mutate(natural_shock = ifelse(p46q2a_ < 4, 1, # natural shock
                                      ifelse(p46q2a_ > 3 & p46q2a_ < 12, 0, NA))) %>% 
        mutate(economic_shock = ifelse(p46q2a_ > 3 & p46q2a_ < 12, 1, # economic shock
                                       ifelse(p46q2a_ < 4, 0, NA))) %>% 
        group_by(tinh_2008, quan_2008, xa_2008, ma_h0_2008) %>% 
        mutate(natural_shock = max(natural_shock)) %>% 
        mutate(economic_shock = max(economic_shock)) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, natural_shock, economic_shock) %>% 
        distinct() %>% 
        right_join(data_2008, by = c("tinh_2008", "quan_2008", "xa_2008", "ma_h0_2008"))


# 18-35 data --------------------------------------------------------------

data_2008 %>% filter(year_born > 1972 & year_born < 1991) %>% 
        ungroup() %>% 
        count(head_relationship)

# Outcome variables - Labor Market Outcomes -------------------------------

outcome_2008 <- as_tibble(read_dta("Q5_New_08.dta", encoding = "VISCII")) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, p27ma_,
                p27ma_, p27q1a_, p27q1b_, p27q1c_, p27q1d_, p27q1e_) %>% 
        rename(wage = p27q1a_,
               agriculture = p27q1b_,
               self_employed = p27q1c_,
               common_property = p27q1d_,
               house_work = p27q1e_)

# Wage
