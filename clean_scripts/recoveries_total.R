# Cumulative Total of Confirmed Recoveries

inf_HH %>%
  transmute(total_of_confirmed_recoveries = cumsum(amount_recovery)) %>%
  slice_tail()

# By Age Group

inf_HH %>%
  group_by(age_group) %>% 
  transmute(total_of_confirmed_recoveries = cumsum(amount_recovery)) %>%
  slice_tail() %>%
  arrange(desc(total_of_confirmed_recoveries))

# By Sex

inf_HH %>%
  group_by(sex) %>% 
  transmute(total_of_confirmed_recoveries = cumsum(amount_recovery)) %>%
  slice_tail() %>% 
  arrange(desc(total_of_confirmed_recoveries))

# Visualised by ref_date

inf_HH %>% select(ref_date, amount_recovery) %>%
  arrange(ref_date) %>%
  mutate(cumulative_recoveries = cumsum(amount_recovery)) %>%
  ggplot(aes(x = ref_date, y = cumulative_recoveries)) + geom_line()

# Visualised by rep_date

inf_HH %>% select(rep_date, amount_recovery) %>%
  arrange(rep_date) %>%
  mutate(cumulative_recoveries = cumsum(amount_recovery)) %>%
  ggplot(aes(x = rep_date, y = cumulative_recoveries)) + geom_line()




