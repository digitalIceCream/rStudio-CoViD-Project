# Cumulative Total of Confirmed Cases

inf_HH_cumtot <- inf_HH %>%
  transmute(total_of_confirmed_cases = cumsum(amount_case)) %>%
  slice_tail()

  ## Visualised by ref_date

inf_HH %>% select(ref_date, amount_case) %>%
  arrange(ref_date) %>%
  mutate(cumulative_cases = cumsum(amount_case)) %>%
  ggplot(aes(x = ref_date, y = cumulative_cases)) + geom_line()

    ### Cumulative Total of Confirmed Cases by Age Bracket (Visualised)

inf_HH %>% select(ref_date, age_group, amount_case) %>%
  arrange(ref_date) %>%
  group_by(age_group) %>% 
  mutate(cumulative_cases = cumsum(amount_case)) %>% 
  ungroup() %>% 
  ggplot(aes(x = ref_date, y = cumulative_cases, fill = age_group)) + geom_area()
 
  ## Visualised by rep_date

inf_HH %>% select(rep_date, amount_case) %>%
  arrange(rep_date) %>% 
  mutate(cumulative_cases = cumsum(amount_case)) %>% 
  ggplot(aes(x = rep_date, y = cumulative_cases)) + geom_line()

# By Age Group

inf_HH %>%
  group_by(age_group) %>% 
  transmute(total_of_confirmed_cases = cumsum(amount_case)) %>%
  slice_tail() %>% 
  arrange(desc(total_of_confirmed_cases))

# By Sex

inf_HH %>%
  group_by(sex) %>% 
  transmute(total_of_confirmed_cases = cumsum(amount_case)) %>%
  slice_tail() %>% 
  arrange(desc(total_of_confirmed_cases))
