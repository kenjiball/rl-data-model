# Load in the historical odds data from aus betting website
install.packages(plyr)

library(XLConnect)
library(jsonlite)
library(plyr)

getwd()
#setwd("C:/Users/kenji/Desktop/NRL/Fox_Sports_Data")
setwd("C:/Users/ballk/Documents/Fox_Sports_Data/Fox_Sports_Data")


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

# Make add  start date time
odds_table$start_datetime <-  as.POSIXct(paste( as.character(odds_table$Date), strftime(odds_table$Kick.off..local., format="%H:%M:%S"), sep=" "))



# Build a Function to load scoring summary data to use to match and add more details

create.matchmatrix <-function(match_vector){
  # Create and empty data frame
  output_df <- data.frame(match_id=character(), round=numeric(),round_name=character(),venue_name=character(),
                               venue_city=character(), start_datetime = character(), start_date = character(),
                               teamA_name=character() ,teamA_table_position=numeric(), teamA_halftime_score=numeric(),
                               teamB_name=character() ,teamB_table_position=numeric(), teamB_halftime_score=numeric(), stringsAsFactors=FALSE )
  scoringdata_df <- data.frame(match_id=character(), round=numeric(),round_name=character(),venue_name=character(),
                               venue_city=character(), start_datetime = character(), start_date = character(),
                               teamA_name=character() ,teamA_table_position=numeric(), teamA_halftime_score=numeric(),
                               teamB_name=character() ,teamB_table_position=numeric(), teamB_halftime_score=numeric(), stringsAsFactors=FALSE )
  
  for(i in 1:length(match_vector)){   

jsonFileName <- match_vector[i]
jsonMatchData <- fromJSON(paste("scoringdata_",jsonFileName, ".json",sep = ""))



# Load fox sports match data into the data frame
scoringdata_df <- as.data.frame(cbind( jsonMatchData$scoring_summary$match_id
                                      ,jsonMatchData$scoring_summary$round$number
                                      , jsonMatchData$scoring_summary$round$name
                                      , jsonMatchData$scoring_summary$venue$name
                                      , jsonMatchData$scoring_summary$venue$city
                                      , jsonMatchData$scoring_summary$match_start_date
                                      , jsonMatchData$scoring_summary$match_start_date
                                      , jsonMatchData$scoring_summary$team_A$name
                                      , jsonMatchData$scoring_summary$team_A$competition_table_position
                                      , jsonMatchData$scoring_summary$team_A$halftime_score
                                      , jsonMatchData$scoring_summary$team_B$name
                                      , jsonMatchData$scoring_summary$team_B$competition_table_position
                                      , jsonMatchData$scoring_summary$team_B$halftime_score))
# Define names
names(scoringdata_df) <- c("match_id","round","round_name","venue_name","venue_city","start_datetime","start_date"
                           ,"teamA_name","teamA_table_position","teamA_halftime_score","teamB_name","teamB_table_position","teamB_halftime_score")

# Fix the date and time formats of start_datetime and start_date
scoringdata_df$start_date <- as.Date(jsonMatchData$scoring_summary$match_start_date)

T_split <- gregexpr(pattern = "T",scoringdata_df$start_datetime)[[1]][1]
match_time <- substr(scoringdata_df$start_datetime,T_split+1,T_split+5)
scoringdata_df$start_datetime <- as.POSIXct(paste( as.character(scoringdata_df$start_date),match_time , sep=" "))

output_df <- rbind(output_df, scoringdata_df )

  }
  
  return(output_df)
}


matchmatrix_season_2017 <- create.matchmatrix(season_2017)



head(matchmatrix_season_2017)

head(scoringdata_df)


head(odds_table)

## merged_data <- merge(matchmatrix_season_2017,odds_table, by.x = c("start_date","teamA_name","teamB_name"), by.y = c("Date","Home.Team","Away.Team"))

head(merged_data)



