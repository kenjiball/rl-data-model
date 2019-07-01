# Get player career summary using by scraping player data using rvest
# Example: https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/


### Examples
#get_player_career_summary("blake", "ferguson", "eels", web_url)
#get_player_career_summary("jesse", "bromwich", "storm", web_url)
#get_player_career_summary("Tom", "Opacic", "cowboys", web_url, player_url = "https://www.cowboys.com.au/teams/telstra-premiership/north-queensland-cowboys/tom-opacic/")
	
get_player_career_summary <- function(player_first_name, player_last_name, player_team, web_url, player_url = NULL){

# Make URL
if(is.null(player_url)){
  url <- paste0(web_url,"/players/nrl-premiership/",player_team,"/",player_first_name,"-",player_last_name,"/")
}else{
  url <- player_url
}

#Reading the HTML code from the website
webpage <- read_html(url)


#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'.o-section--half-gutter:nth-child(6) .table__cell--fixed~ .table__th , .o-section--half-gutter:nth-child(6) .table-tbody__td')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)


#Data-Preprocessing: removing '\n'
rank_data<-gsub("\n","",rank_data)
rank_data<-gsub("\r","",rank_data)
rank_data<-gsub(" ","",rank_data)
rank_data<-gsub("\u00A0"," ",rank_data)
rank_data<-gsub("-","0",rank_data)
rank_data<-gsub("%","",rank_data)
rank_data<-gsub(",","",rank_data)

# Convert to data frame
rank_data_df <- data.frame(rank_data)
rank_data_df$rank_data <- as.character(rank_data_df$rank_data)

# add in team name
rank_data_df[1,] <- "Team"

# Remove blank rows
rank_data_df <- rank_data_df %>% filter(rank_data != "" )


# Find how many seasons and frame width of data in table
frame_width <- which(rank_data_df$rank_data == "Average Points")
seasons <- length(rank_data_df$rank_data)/frame_width


# Loop through seasons and build the data frame
for(i in 1:seasons){
    n_start <- (i-1)*frame_width+1  
    n_end <- i*frame_width
    n_slice <- n_start:n_end
    
    if(i == 1){
      output_df <- slice(rank_data_df, n_slice)
    }else{
      slice_df <- slice(rank_data_df, n_slice)
      output_df <- bind_cols(output_df, slice_df)
      rm(slice_df)
    }
}

# Get col names and remove from df
col_names <- output_df$rank_data
output_df$rank_data <- NULL

# Transpose dataframe
player_df <- as.data.frame(t(output_df))

# Set Col Names and remove Row Names
colnames(player_df) <- col_names
rownames(player_df) <- NULL

# insert first and last names
player_df$firstName <- str_to_title(player_first_name)
player_df$lastName <- str_to_title(player_last_name)

player_df <- player_df %>% select(firstName, lastName, everything())


return(player_df)

}

