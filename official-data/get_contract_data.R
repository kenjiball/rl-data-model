# Get player contracts
# Currently contract lengths are published on NRL.com
# https://www.nrl.com/news/2019/01/23/2019-nrl-signings-player-transfers-and-contracts/

# Make URL
url <- "https://www.nrl.com/news/2019/01/23/2019-nrl-signings-player-transfers-and-contracts/"


#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
contract_html <- html_nodes(webpage,'td , th')

#Converting the ranking data to text
contract_data <- html_text(contract_html)

#Data-Preprocessing: removing '\n'
contract_data<-gsub("\n","",contract_data)
contract_data<-gsub("\r","",contract_data)
contract_data<-gsub(" ","",contract_data)
contract_data<-gsub("\u00A0","",contract_data)

# Convert to data frame
contract_data_df <- data.frame(contract_data)
contract_data_df$contract_data <- as.character(contract_data_df$contract_data)

# Remove blank rows
contract_data_df <- contract_data_df %>% filter(!contract_data %in% c(""," ") )

# Add in a team id to help plit the data
contract_data_df2 <- contract_data_df %>% 
  mutate( team_start = if_else(contract_data == "Player", 1, 0),
          team_order = cumsum(team_start),
          fox_teamId = case_when(team_order == 1 ~ 55016,
                            team_order == 2 ~ 55004,
                            team_order == 3 ~ 55005,
                            team_order == 4 ~ 55006,
                            team_order == 5 ~ 55044,
                            team_order == 6 ~ 55009,
                            team_order == 7 ~ 55015,
                            team_order == 8 ~ 55007,
                            team_order == 9 ~ 55003,
                            team_order == 10 ~ 55008,
                            team_order == 11 ~ 55010,
                            team_order == 12 ~ 55011,
                            team_order == 13 ~ 55012,
                            team_order == 14 ~ 55017,
                            team_order == 15 ~ 55013,
                            team_order == 16 ~ 55014,
                            TRUE ~ 0),
          player_start = if_else(lead(contract_data, n = 1) %in% c("2019") , 1, 0),
          player_order = cumsum(player_start),
          option_type = if_else(contract_data %in% c("CO","MO","PO"), contract_data, "NA")
  ) %>% 
  group_by(player_order, option_type) %>% 
  mutate( option_term = if_else(contract_data %in% c("CO","MO","PO"), 1, 0),
          option_term = cumsum(option_term)
          
  ) %>% 
  ungroup() %>% 
  mutate(
    option_year = case_when(option_term == 0 ~ 0,
                            option_term == 1 ~ as.numeric(lag(contract_data, n = 1)) + 1,
                            option_term == 2 ~ as.numeric(lag(contract_data, n = 2)) + 2,
                            option_term == 3 ~ as.numeric(lag(contract_data, n = 3)) + 3),
    
    player_name  = if_else(player_start == 1, contract_data, "NA"),
    
    "2019"      = case_when(player_start == 1 & lead(contract_data, n = 1) == "2019" ~ "Y",
                            player_start == 1 & lead(option_year,   n = 1) == "2019" ~ lead(option_type,   n = 1), 
                            TRUE ~ "N"),
    "2020"      = case_when(player_start == 1 & lead(contract_data, n = 2) == "2020" ~ "Y",
                            player_start == 1 & lead(option_year,   n = 2) == "2020" ~ lead(option_type,   n = 2), 
                            TRUE ~ "N"),
    "2021"      = case_when(player_start == 1 & lead(contract_data, n = 3) == "2021" ~ "Y",
                            player_start == 1 & lead(option_year,   n = 3) == "2021" ~ lead(option_type,   n = 3), 
                            TRUE ~ "N"),
    "2022"      = case_when(player_start == 1 & lead(contract_data, n = 4) == "2022" ~ "Y",
                            player_start == 1 & lead(option_year,   n = 4) == "2022" ~ lead(option_type,   n = 4), 
                            TRUE ~ "N"),
    "2023"      = case_when(player_start == 1 & lead(contract_data, n = 5) == "2023" ~ "Y",
                            player_start == 1 & lead(option_year,   n = 5) == "2023" ~ lead(option_type,   n = 5), 
                            TRUE ~ "N"),
    "2024"      = case_when(player_start == 1 & lead(contract_data, n = 6) == "2024" ~ "Y",
                            player_start == 1 & lead(option_year,   n = 6) == "2024" ~ lead(option_type,   n = 6), 
                            TRUE ~ "N"),
    "2025"      = case_when(player_start == 1 & lead(contract_data, n = 7) == "2025" ~ "Y",
                            player_start == 1 & lead(option_year,   n = 7) == "2025" ~ lead(option_type,   n = 7), 
                            TRUE ~ "N"),
    "2026"      = case_when(player_start == 1 & lead(contract_data, n = 8) == "2026" ~ "Y",
                            player_start == 1 & lead(option_year,   n = 8) == "2026" ~ lead(option_type,   n = 8), 
                            TRUE ~ "N"),
    "2027"      = case_when(player_start == 1 & lead(contract_data, n = 9) == "2027" ~ "Y",
                            player_start == 1 & lead(option_year,   n = 9) == "2027" ~ lead(option_type,   n = 9), 
                            TRUE ~ "N")
 
    
  ) %>% 
  filter(!contract_data %in% c("Player", "Contractedseasons","CO","MO","PO")) %>% 
  filter(!contract_data %in% 2019:2027) %>%
  left_join(team_names_df, by = "fox_teamId") %>% 
  select(fox_teamId, nrl_team_short_name, player_order, player_name, "2019", "2020", "2021", "2022", "2023", "2024", "2025", "2026", "2027")

















