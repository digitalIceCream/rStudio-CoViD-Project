df_sex_ref_date_recoveries <- function(){
  
  M <- inf_HH %>% select(ref_date, sex, amount_recovery) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, sex) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(sex == "M") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  W <- inf_HH %>% select(ref_date, sex, amount_recovery) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, sex) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(sex == "W") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  unbekannt <- inf_HH %>% select(ref_date, sex, amount_recovery) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, sex) %>% 
    summarise(amount_recovery = sum(amount_recovery)) %>% 
    ungroup %>% 
    filter(sex == "unbekannt") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_recovery))
  
  rbind(M, W, unbekannt)
}
