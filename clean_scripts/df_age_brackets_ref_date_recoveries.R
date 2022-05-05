df_age_brackets_ref_date_recoveries <- function(){
  A0 <- inf_HH %>% select(ref_date, age_group, amount_recovery) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(age_group == "unbekannt") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  A1 <- inf_HH %>% select(ref_date, age_group, amount_recovery) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(age_group == "A00-A04") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  A2 <- inf_HH %>% select(ref_date, age_group, amount_recovery) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(age_group == "A05-A14") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  A3 <- inf_HH %>% select(ref_date, age_group, amount_recovery) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(age_group == "A15-A34") %>%
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  A4 <- inf_HH %>% select(ref_date, age_group, amount_recovery) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(age_group == "A35-A59") %>%
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  A5 <- inf_HH %>% select(ref_date, age_group, amount_recovery) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(age_group == "A60-A79") %>%
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  A6 <- inf_HH %>% select(ref_date, age_group, amount_recovery) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(age_group == "A80+") %>%
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  rbind(A0, A1, A2, A3, A4, A5, A6)
}
