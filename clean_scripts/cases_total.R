# Cumulative Total of Confirmed Cases

inf_HH %>%
  transmute(total_of_confirmed_cases = sum(amount_case)) %>%
  slice_tail()

  ## Visualised by ref_date

inf_HH %>% select(ref_date, amount_case) %>%
  arrange(ref_date) %>% 
  group_by(ref_date) %>% 
  summarise(amount_case = sum(amount_case)) %>%
  mutate(cumulative_cases = cumsum(amount_case)) %>%
  ggplot(aes(ref_date, cumulative_cases)) + geom_area()

  ## Visualised by rep_date

inf_HH %>% select(rep_date, amount_case) %>%
  arrange(rep_date) %>% 
  group_by(rep_date) %>% 
  summarise(amount_case = sum(amount_case)) %>%
  mutate(cumulative_cases = cumsum(amount_case)) %>%
  ggplot(aes(rep_date, cumulative_cases)) + geom_area()

# Cumulative Total of Confirmed Cases By Age Bracket

inf_HH %>%
  group_by(age_group) %>% 
  transmute(total_of_confirmed_cases = cumsum(amount_case)) %>%
  slice_tail() %>% 
  arrange(desc(total_of_confirmed_cases))

  ## Cumulative Total of Confirmed Cases by Age Bracket (Visualised) by ref_date

inf_HH %>% select(ref_date, age_group, amount_case) %>%
  arrange(ref_date) %>%
  group_by(age_group) %>% 
  mutate(cumulative_cases = cumsum(amount_case)) %>% 
  ungroup() %>% 
  ggplot(aes(x = ref_date, y = cumulative_cases, fill = age_group)) + geom_area()

  ## Cumulative Total of Confirmed Cases by Age Bracket (Visualised) by rep_date

inf_HH %>% select(rep_date, age_group, amount_case) %>%
  group_by(age_group) %>%
  arrange(rep_date) %>%
  mutate(case_totals_by_age_group = cumsum(amount_case)) %>%
  ggplot(aes(x = rep_date, y = case_totals_by_age_group, fill = age_group)) +
  geom_area()
 
  ## Total of Confirmed Cases by Age Bracket (Visualised)

inf_HH %>% select(age_group, amount_case) %>%
  group_by(age_group) %>%
  summarise(sum = sum(amount_case)) %>%
  ggplot(aes(fct_reorder(age_group, sum), sum)) + geom_col()

# Total of Confirmed Cases by Sex

inf_HH %>%
  group_by(sex) %>% 
  transmute(total_of_confirmed_cases = cumsum(amount_case)) %>%
  slice_tail() %>% 
  arrange(desc(total_of_confirmed_cases))
