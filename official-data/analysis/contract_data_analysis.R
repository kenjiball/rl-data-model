# June 30 Contract Analysis

# contract_data_df3
# Or load from read_csv("./data/player_contracts_20190701.csv")


# summary of number of contracted players by year.

contract_summary <- contract_data_df3 %>% 
  select(nrl_team_short_name, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`, `2025`, `2026`, `2027`) %>% 
  gather(year, contract_status, -nrl_team_short_name) %>% 
  group_by(nrl_team_short_name, year) %>% 
  summarise( count = sum(if_else(contract_status == "Y",1,0), na.rm = TRUE) ) %>% 
  ungroup %>% 
  spread(year, count)


# Player on the longest contract for each team.

longest_term <- contract_data_df3 %>% 
  select(nrl_team_short_name, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`, `2025`, `2026`, `2027`) %>% 
  gather(year, contract_status, -nrl_team_short_name) %>% 
  filter(contract_status == "Y") %>% 
  group_by(nrl_team_short_name) %>% 
  summarise( year = max(year, na.rm = TRUE))
  
contract_summary <- contract_data_df3 %>% 
  select(nrl_team_short_name, player_name, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`, `2025`, `2026`, `2027`) %>% 
  gather(year, contract_status, -nrl_team_short_name, -player_name) %>% 
  filter(contract_status == "Y") %>% 
  semi_join(longest_term, by = c("nrl_team_short_name", "year"))




