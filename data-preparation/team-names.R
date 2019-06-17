# Mapping functions
# Map player and team data using 2018 data

# Get fox team names
# Case statement to map Tigers team name for the join
fox_team_names <- season_2018_datamatrix %>% 
  select(for_team_id, for_team_code, for_name, for_short_name) %>% 
  mutate( map_name = case_when( for_short_name == "Tigers" ~ "Wests Tigers",
                                TRUE ~ for_short_name
  ) ) %>% 
  unique() %>% 
  rename( "fox_teamId" = for_team_id, "fox_team_code" = for_team_code, "fox_team_name" = for_name, "fox_team_short_name" = for_short_name)

# Get NRL team names
nrl_team_names <- match_table_2018_df %>% 
  select(homeId, homeNickName, homeTeam) %>% 
  unique() %>% 
  rename( "nrl_teamId" = homeId, "nrl_team_name" = homeTeam, "nrl_team_short_name" = homeNickName)

# Join data frames to create mapping table
team_names_df <- left_join(nrl_team_names, fox_team_names, by = c("nrl_team_short_name" = "map_name"))





# OLD Script for mapping team id to name and code
# Can be deleted once everything is mapped as per above code


team_name_df <-
data.frame(rbind(
      c(55003,"WAR","Warriors","New Zealand Warriors",""),
      c(55004,"CBR","Canberra","Canberra Raiders",""),
      c(55005,"CBY","Canterbury","Canterbury Bulldogs","Canterbury-Bankstown Bulldogs"),
      c(55006,"CRO","Cronulla","Cronulla Sharks","Cronulla-Sutherland Sharks"),
      c(55007,"NEW","Newcastle","Newcastle Knights",""),
      c(55008,"NQL","North Queensland","North Queensland Cowboys","North QLD Cowboys"),
      c(55009,"MAN","Manly","Manly Sea Eagles","Manly-Warringah Sea Eagles"),
      c(55010,"PAR","Parramatta","Parramatta Eels",""),
      c(55011,"PEN","Penrith","Penrith Panthers",""),
      c(55012,"STI","St George Illawarra","St. George Illawarra Dragons","St George Dragons"),
      c(55013,"SYD","Sydney Roosters","Sydney Roosters",""),
      c(55014,"WST","Wests Tigers","Wests Tigers",""),
      c(55015,"MEL","Melbourne","Melbourne Storm",""),
      c(55016,"BRI","Brisbane","Brisbane Broncos",""),
      c(55017,"SOU","South Sydney","South Sydney Rabbitohs",""),
      c(55044,"GLD","Gold Coast Titans","Gold Coast Titans","")
  
))

names(team_name_df) <- c("team_id", "team_code", "team_name", "team_name_long", "team_name_alternate")

team_name_df
