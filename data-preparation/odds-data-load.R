# Load in the historical odds data from aus betting website
install.packages(plyr)

library(XLConnect)
library(jsonlite)
library(dplyr)
library(lubridate)


# Set working directory
setwd(wd)
getwd()



# Load worksheet
workbook_name <- "../nrl.xlsx"
odds_workbook <- loadWorkbook(workbook_name, create = TRUE)
sheet_name <- "Data"
odds_table <- readWorksheet(odds_workbook, sheet_name, header = TRUE, startRow = 2)

# Map team names to match the fox sports data
odds_table$Home.Team <- gsub("Brisbane Broncos", "Brisbane", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Canberra Raiders", "Canberra", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Canterbury Bulldogs", "Canterbury", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Canterbury-Bankstown Bulldogs", "Canterbury", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Cronulla Sharks", "Cronulla", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Cronulla-Sutherland Sharks", "Cronulla", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Gold Coast Titans", "Gold Coast Titans", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Manly Sea Eagles", "Manly", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Manly-Warringah Sea Eagles", "Manly", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Melbourne Storm", "Melbourne", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("New Zealand Warriors", "Warriors", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Newcastle Knights", "Newcastle", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("North QLD Cowboys", "North Queensland", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("North Queensland Cowboys", "North Queensland", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Parramatta Eels", "Parramatta", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Penrith Panthers", "Penrith", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("South Sydney Rabbitohs", "South Sydney", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("St George Dragons", "St George Illawarra", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("St. George Illawarra Dragons", "St George Illawarra", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Sydney Roosters", "Sydney Roosters", odds_table$Home.Team, fixed=TRUE)
odds_table$Home.Team <- gsub("Wests Tigers", "Wests Tigers", odds_table$Home.Team, fixed=TRUE)

odds_table$Away.Team <- gsub("Brisbane Broncos", "Brisbane", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Canberra Raiders", "Canberra", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Canterbury Bulldogs", "Canterbury", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Canterbury-Bankstown Bulldogs", "Canterbury", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Cronulla Sharks", "Cronulla", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Cronulla-Sutherland Sharks", "Cronulla", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Gold Coast Titans", "Gold Coast Titans", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Manly Sea Eagles", "Manly", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Manly-Warringah Sea Eagles", "Manly", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Melbourne Storm", "Melbourne", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("New Zealand Warriors", "Warriors", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Newcastle Knights", "Newcastle", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("North QLD Cowboys", "North Queensland", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("North Queensland Cowboys", "North Queensland", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Parramatta Eels", "Parramatta", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Penrith Panthers", "Penrith", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("South Sydney Rabbitohs", "South Sydney", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("St George Dragons", "St George Illawarra", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("St. George Illawarra Dragons", "St George Illawarra", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Sydney Roosters", "Sydney Roosters", odds_table$Away.Team, fixed=TRUE)
odds_table$Away.Team <- gsub("Wests Tigers", "Wests Tigers", odds_table$Away.Team, fixed=TRUE)

# Rename columns
names(odds_table)[names(odds_table) == "Date"] <- "match_start_date"
names(odds_table)[names(odds_table) == "Home.Team"] <- "for_team_name"
names(odds_table)[names(odds_table) == "Away.Team"] <- "against_team_name"
names(odds_table)[names(odds_table) == "Kick.off..local."] <- "start_time"

# Mutate to create new features


#### Fix date formatting and start date time
odds_table_2 <- odds_table %>% mutate(
  start_date = floor_date(as.POSIXct(ymd(odds_table$match_start_date)),"day"),
  start_time = hms(strftime(odds_table$start_time, format="%H:%M:%S")),
  start_timedate = force_tz(ymd_hms(paste(match_start_date,start_time)), tzone = "Australia/Sydney")
)


#### Merge to season_all_matchmatrix
col_args_1 <- season_all_matchmatrix %>% arrange(desc(match_start_date)) 
col_args_2 <- odds_table_2 %>% arrange(desc(match_start_date))
merged_data <- col_args_1 %>% inner_join(col_args_2, by = c("start_date","for_team_name","against_team_name") )

head(merged_data)
dim(merged_data)


write.csv(merged_data,file="../merged_data.csv")



# TESTING Basic PLots
ggplot(merged_data, aes(x = Home.Line.Close, fill = for_match_result)) +
  geom_histogram(binwidth = 4) +
  facet_grid(for_match_result ~ .)

merged_data %>%
  select(season, Home.Line.Close, for_score, against_score) %>%
  mutate(Line.Interval = findInterval(Home.Line.Close, vec = -6:6*4),
         Line.Interval.Index = findInterval(Home.Line.Close, vec = -6:6*4) 
         ) %>%
  
  
  ggplot(aes(x = Home.Line.Close, y = (for_score - against_score) )) +
  geom_point()

    

  table(findInterval(merged_data$Home.Line.Close, vec = -6:6*4))


# TESTING MERGE: 
# Merge to season_all_matchmatrix
col_args_1 <- season_all_matchmatrix %>% 
  filter(season == 2018, round == 18 ) %>% 
  select(match_start_date, start_date, start_datetime, for_team_name, against_team_name) %>% 
  arrange(desc(start_datetime)) 

col_args_2 <- odds_table_2 %>% 
  filter(match_start_date >= "2018-07-13" & match_start_date <= "2018-07-16" ) %>% 
  select(match_start_date, start_date, start_time, for_team_name, against_team_name) %>%
  arrange(desc(start_date, start_time))


merged_data <- col_args_1 %>% inner_join(col_args_2, by = c("start_date","for_team_name","against_team_name") )

head(col_args_1)
head(col_args_2)
head(merged_data)

dim(col_args_1)
dim(col_args_2)
dim(merged_data)

class(col_args_1$start_date)
class(col_args_2$start_date)


head(col_args_1$start_date)
head(col_args_2$start_date)

