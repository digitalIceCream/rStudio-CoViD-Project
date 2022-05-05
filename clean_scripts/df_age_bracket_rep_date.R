df_age_brackets_rep_date <- function(){
  A0 <- inf_HH %>% select(rep_date, age_group, amount_case) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, age_group) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(age_group == "unbekannt") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_case))
  
  A1 <- inf_HH %>% select(rep_date, age_group, amount_case) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, age_group) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(age_group == "A00-A04") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_case))
  
  A2 <- inf_HH %>% select(rep_date, age_group, amount_case) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, age_group) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(age_group == "A05-A14") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_case))
  
  A3 <- inf_HH %>% select(rep_date, age_group, amount_case) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, age_group) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(age_group == "A15-A34") %>%
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_case))
  
  A4 <- inf_HH %>% select(rep_date, age_group, amount_case) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, age_group) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(age_group == "A35-A59") %>%
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_case))
  
  A5 <- inf_HH %>% select(rep_date, age_group, amount_case) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, age_group) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(age_group == "A60-A79") %>%
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_case))
  
  A6 <- inf_HH %>% select(rep_date, age_group, amount_case) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, age_group) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(age_group == "A80+") %>%
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_case))
  
  rbind(A0, A1, A2, A3, A4, A5, A6)
}
