df_sex_rep_date <- function(){
  
  M <- inf_HH %>% select(rep_date, sex, amount_case) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, sex) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(sex == "M") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_case))
  
  W <- inf_HH %>% select(rep_date, sex, amount_case) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, sex) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(sex == "W") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_case))
  
  unbekannt <- inf_HH %>% select(rep_date, sex, amount_case) %>% 
    arrange(rep_date) %>% 
    group_by(rep_date, sex) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(sex == "unbekannt") %>% 
    arrange(rep_date) %>% 
    mutate(cum_cases = cumsum(amount_case))
  
  df_sex_rep_date <- rbind(M, W, unbekannt)
}
