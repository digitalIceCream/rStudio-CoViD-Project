# Cumulative Total of Confirmed Deaths

inf_HH %>%
  transmute(total_of_confirmed_deaths = cumsum(amount_death)) %>%
  slice_tail()

# By Age Group

inf_HH %>% 
  group_by(age_group) %>% 
  transmute(total_of_confirmed_deaths = cumsum(amount_death)) %>%
  slice_tail() %>% 
  arrange(desc(total_of_confirmed_deaths))

# By Sex

inf_HH %>%
  group_by(sex) %>% 
  transmute(total_of_confirmed_deaths = cumsum(amount_death)) %>%
  slice_tail() %>% 
  arrange(desc(total))

# Visualised by ref_date

inf_HH %>% select(ref_date, amount_death) %>%
  arrange(ref_date) %>%
  mutate(cumulative_deaths = cumsum(amount_death)) %>%
  ggplot(aes(x = ref_date, y = cumulative_deaths)) + geom_line()

# Visualised by rep_date

inf_HH %>% select(rep_date, amount_death) %>%
  arrange(rep_date) %>%
  mutate(cumulative_deaths = cumsum(amount_death)) %>%
  ggplot(aes(x = rep_date, y = cumulative_deaths)) + geom_line()

