##### OUTCOME VARIABLES 2008 #####

rm(list = ls())
library(stargazer)
library(Hmisc)
library(tidyverse)
library(haven)
library(zoo)
library(dplyr)

# Set working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/PhD/Accessibility and Labor Market Outcomes")

# 6-18 data - 2008--------------------------------------------------------------

outcome_2008 <- as_tibble(read_dta("2008_Q1a_New.dta", encoding="VISCII")) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, p2stt_, #individual identity variables
               p2q4_ # year born
        ) %>% 
        rename(year_born = p2q4_,
               hhid = p2stt_) %>% 
        filter(year_born < 2001 & year_born > 1989) # Select people from 6 to 18 years old

# Outcome variables - Labor Market Outcomes

# Work dummy

outcome_2008 <- as_tibble(read_dta("Q5_New_08.dta", encoding = "VISCII")) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008,
               p27ma_, p27q1a_, p27q1b_, p27q1c_, p27q1d_, p27q1e_) %>% 
        rename(hhid = p27ma_,
               wage = p27q1a_,
               agriculture = p27q1b_,
               self_employed = p27q1c_,
               common_property = p27q1d_,
               house_work = p27q1e_) %>% 
        right_join(outcome_2008, 
                    by = c("tinh_2008", "quan_2008", "xa_2008", "ma_h0_2008", "hhid"))

# Days worked wage

outcome_2008 <- as_tibble(read_dta("Q5a_New.dta", encoding = "VISCII")) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008,
               p28ma_, p28q6_, p28q7_) %>% 
        dplyr::rename(hhid = p28ma_) %>% 
        mutate(day_wage = p28q6_*p28q7_) %>% 
        group_by(tinh_2008, quan_2008, xa_2008, ma_h0_2008, hhid) %>% 
        mutate(day_wage = sum(day_wage, na.rm = T)) %>% 
        dplyr::select(-c(p28q6_, p28q7_)) %>% 
        ungroup() %>% 
        distinct() %>% 
        right_join(outcome_2008, 
                   by = c("tinh_2008", "quan_2008", "xa_2008", "ma_h0_2008", "hhid")) %>% 
        mutate(day_wage = ifelse(wage == 2, 0, day_wage))

# Days worked agriculture

outcome_2008 <- as_tibble(read_dta("Q5b_New.dta", encoding = "VISCII")) %>% 
        dplyr::select(tinh_2008, quan_2008, xa_2008, ma_h0_2008,
               p29ma_, matches("p29q1")) %>% 
        dplyr::rename(hhid = p29ma_) %>% 
        mutate(day_agr = rowSums(.[, c(6:11)], na.rm = T)) %>% 
        dplyr::select(tinh_2008, quan_2008, xa_2008, ma_h0_2008,
                      hhid, day_agr) %>% 
        distinct() %>% 
        right_join(outcome_2008, 
                   by = c("tinh_2008", "quan_2008", "xa_2008", "ma_h0_2008", "hhid")) %>% 
        mutate(day_agr = ifelse(agriculture == 2, 0, ifelse(day_agr > 365, 365, day_agr)))

# Days worked hh enterprise

outcome_2008 <- as_tibble(read_dta("Q5c2_New.dta", encoding = "VISCII")) %>% 
        dplyr::select(tinh_2008, quan_2008, xa_2008, ma_h0_2008,
                      p32ma_, matches("p32q1")) %>% 
        dplyr::rename(hhid = p32ma_) %>% 
        mutate(day_enter = rowSums(.[, c(6:9)], na.rm = T)) %>% 
        dplyr::select(tinh_2008, quan_2008, xa_2008, ma_h0_2008,
                      hhid, day_enter) %>% 
        distinct() %>% 
        right_join(outcome_2008, 
                   by = c("tinh_2008", "quan_2008", "xa_2008", "ma_h0_2008", "hhid")) %>% 
        mutate(day_enter = ifelse(self_employed == 2, 0, ifelse(day_enter > 365, 365, day_enter)))

# Total days worked

outcome_2008 <- outcome_2008 %>% mutate(total_day_worked = (rowSums(outcome_2008 %>% 
                                                    dplyr::select(matches("day")),
                                                    na.rm = T))) %>% 
        mutate(total_day_worked = ifelse(total_day_worked > 365, 365, total_day_worked))


# 6-18 data - 2010 --------------------------------------------------------

outcome_2010 <- as_tibble(read_dta("Q1_New_10.dta", encoding="VISCII")) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008,
               tinh_2010, quan_2010, xa_2010, ma_h0_2010, p1stt_, #individual identity variables
               p1q4_ # year born
        ) %>% 
        rename(year_born = p1q4_,
               hhid = p1stt_) %>% 
        filter(year_born < 2003 & year_born > 1991) # Select people from 6 to 18 years old

# Outcome variables - Labor Market Outcomes 

# Work dummy

outcome_2010 <- as_tibble(read_dta("Q5_New_10.dta", encoding = "VISCII")) %>% 
        select(tinh_2010, quan_2010, xa_2010, ma_h0_2010, 
               p26ma_, p26q1a_, p26q1b_, p26q1c_, p26q1d_, p26q1e_) %>% 
        rename(hhid = p26ma_,
               wage = p26q1a_,
               agriculture = p26q1b_,
               self_employed = p26q1c_,
               common_property = p26q1d_,
               house_work = p26q1e_) %>% 
        right_join(outcome_2010, 
                   by = c("tinh_2010", "quan_2010", "xa_2010", "ma_h0_2010", "hhid"))
