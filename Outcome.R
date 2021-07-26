# 6-18 data --------------------------------------------------------------

outcome_2008 <- as_tibble(read_dta("2008_Q1a_New.dta", encoding="VISCII")) %>% 
        select(tinh_2008, quan_2008, xa_2008, ma_h0_2008, p2stt_, #individual identity variables
               p2q2_, p2q4_ # individual characteristics
        ) %>% 
        rename(head_relationship = p2q2_ ,
               year_born = p2q4_,
               hhid = p2stt_) %>% 
        filter(year_born > 2001 & year_born < 1989) %>% # Select people from 6 to 18 years old
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