df_sex_rep_date_deaths <- function(){
  
  M <- inf_HH %>% select(rep_date, sex, amount_death) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, sex) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(sex == "M") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  W <- inf_HH %>% select(rep_date, sex, amount_death) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, sex) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(sex == "W") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  unbekannt <- inf_HH %>% select(rep_date, sex, amount_death) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, sex) %>% 
    summarise(amount_death = sum(amount_death)) %>% 
    ungroup %>% 
    filter(sex == "unbekannt") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_death))
  
  rbind(M, W, unbekannt)
}
