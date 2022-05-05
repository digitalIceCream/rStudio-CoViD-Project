df_sex_ref_date <- function(){
  
  M <- inf_HH %>% select(ref_date, sex, amount_case) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, sex) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(sex == "M") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_case))

  W <- inf_HH %>% select(ref_date, sex, amount_case) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, sex) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(sex == "W") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_case))

  unbekannt <- inf_HH %>% select(ref_date, sex, amount_case) %>% 
    arrange(ref_date) %>% 
    group_by(ref_date, sex) %>% 
    summarise(amount_case = sum(amount_case)) %>% 
    ungroup %>% 
    filter(sex == "unbekannt") %>% 
    arrange(ref_date) %>% 
    mutate(cum_cases = cumsum(amount_case))

  df_sex_ref_date <- rbind(M, W, unbekannt)
}
