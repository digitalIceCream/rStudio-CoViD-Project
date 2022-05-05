df_sex_rep_date_recoveries <- function(){
  
  M <- inf_HH %>% select(rep_date, sex, amount_recovery) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, sex) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(sex == "M") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  W <- inf_HH %>% select(rep_date, sex, amount_recovery) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, sex) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(sex == "W") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  unbekannt <- inf_HH %>% select(rep_date, sex, amount_recovery) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, sex) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(sex == "unbekannt") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  rbind(M, W, unbekannt)
}
