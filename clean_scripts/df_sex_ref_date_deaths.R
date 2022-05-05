df_sex_ref_date_deaths <- function(){
  
  M <- inf_HH %>% select(ref_date, sex, amount_death) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, sex) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(sex == "M") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  W <- inf_HH %>% select(ref_date, sex, amount_death) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, sex) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(sex == "W") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  unbekannt <- inf_HH %>% select(ref_date, sex, amount_death) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, sex) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(sex == "unbekannt") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
    rbind(M, W, unbekannt)
}
