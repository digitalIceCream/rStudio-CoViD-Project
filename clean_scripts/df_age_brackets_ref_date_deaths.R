df_age_brackets_ref_date_deaths <- function(){
  A0 <- inf_HH %>% select(ref_date, age_group, amount_death) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(age_group == "unbekannt") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  A1 <- inf_HH %>% select(ref_date, age_group, amount_death) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(age_group == "A00-A04") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  A2 <- inf_HH %>% select(ref_date, age_group, amount_death) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(age_group == "A05-A14") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  A3 <- inf_HH %>% select(ref_date, age_group, amount_death) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(age_group == "A15-A34") %>%
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  A4 <- inf_HH %>% select(ref_date, age_group, amount_death) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(age_group == "A35-A59") %>%
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  A5 <- inf_HH %>% select(ref_date, age_group, amount_death) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(age_group == "A60-A79") %>%
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  A6 <- inf_HH %>% select(ref_date, age_group, amount_death) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, age_group) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(age_group == "A80+") %>%
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  rbind(A0, A1, A2, A3, A4, A5, A6)
}
