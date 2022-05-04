# Currently new cases by ref_date

inf_HH %>% select(ref_date, new_case, starts_with("amount")) %>% 
  filter(new_case != 0 & amount_death == 0 & amount_recovery == 0) %>% 
  select(ref_date, amount_case) %>%
  group_by(ref_date) %>% 
  summarise(amount_case = sum(amount_case)) %>%
  arrange(desc(ref_date))

# Currently new cases by rep_date

inf_HH %>% select(rep_date, new_case, starts_with("amount")) %>% 
  filter(new_case != 0 & amount_death == 0 & amount_recovery == 0) %>% 
  select(rep_date, ends_with("case"))%>%
  group_by(rep_date) %>% 
  summarise(amount_case = sum(amount_case)) %>% 
  arrange(desc(rep_date))

# Visualised by ref_date

inf_HH_new_cases %>% select(ref_date, starts_with("amount")) %>%
  filter(amount_death == 0 & amount_recovery == 0) %>%
  select(ref_date, amount_case) %>%
  group_by(ref_date) %>%
  summarise(amount_case = sum(amount_case)) %>%
  arrange(desc(ref_date)) %>%
  ggplot(aes(x = ref_date, y = amount_case)) + geom_line()

# Visualised by rep_date

inf_HH_new_cases %>% select(rep_date, starts_with("amount")) %>%
  filter(amount_death == 0 & amount_recovery == 0) %>%
  select(rep_date, amount_case) %>%
  group_by(rep_date) %>%
  summarise(amount_case = sum(amount_case)) %>%
  arrange(desc(rep_date)) %>%
  ggplot(aes(x = rep_date, y = amount_case)) + geom_line()

# Currently new deaths by ref_date

inf_HH %>% select(ref_date, new_case, amount_death) %>% 
  filter(new_case != 0) %>% 
  select(ref_date, amount_death) %>%
  group_by(ref_date) %>% 
  summarise(amount_death = sum(amount_death)) %>%
  arrange(desc(ref_date))

# Currently new deaths by rep_date

inf_HH %>% select(rep_date, new_case, amount_death) %>% 
  filter(new_case != 0) %>% 
  select(rep_date, amount_death) %>%
  group_by(rep_date) %>% 
  summarise(amount_death = sum(amount_death)) %>%
  arrange(desc(rep_date))

# 