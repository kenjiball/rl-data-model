library(r2d3)
library(dplyr)
library(ggplot2)
library(devtools)


# Data preparation
plot_data <- data.frame(season_all_datamatrix$for_name
                        ,season_all_datamatrix$season
                        ,as.numeric(season_all_datamatrix$round)
                        ,season_all_datamatrix$for_complete_sets
                        ,season_all_datamatrix$for_total_sets
                        ,season_all_datamatrix$for_match_result
                        ,season_all_datamatrix$for_points
                        ,season_all_datamatrix$against_points
                        )
names(plot_data) <- c("team","season","round","for_completed_sets","for_total_sets","for_match_result","for_points","against_points")

# Prepare Season Win Totals for Aggregation
plot_data$for_match_result <- gsub("Win",2,plot_data$for_match_result)
plot_data$for_match_result <- gsub("Lose",0,plot_data$for_match_result)
plot_data$for_match_result <- gsub("Draw",1,plot_data$for_match_result)
plot_data$for_match_result <- (as.numeric(plot_data$for_match_result))
names(plot_data$for_match_result) <- "for_match_result"

sapply(plot_data,class)


# Aggregate data by team by year
# Filter out Finals (keep just regular season)
plot_data2 <- plot_data %>% filter(round <= 26)
plot_data3 <- plot_data2 %>% group_by(team, season) %>%
                             summarise(completed_sets = sum(for_completed_sets, na.rm = TRUE)
                                      ,total_sets = sum(for_total_sets, na.rm = TRUE)
                                      ,competition_points = sum(for_match_result, na.rm = TRUE)
                                      ,for_points = sum(for_points, na.rm = TRUE)
                                      ,against_points = sum(against_points, na.rm = TRUE))

# Mutate to include the completion rate
plot_data4 <- plot_data3 %>% mutate(completion_rate = completed_sets/total_sets
                                    , for_against = for_points/against_points
                                    , team_season = paste(team, season, sep = "_"))
plot_data4$completion_rate <- round(plot_data4$completion_rate,4)
plot_data4$for_against <- round(plot_data4$for_against,4)
plot_data5 <- plot_data4 %>% arrange(completion_rate)


head(plot_data5,10)


# Plot
theme_set(theme_bw())

ggplot(plot_data5, aes(x=reorder(team_season,completion_rate,sum), y=completion_rate, label=completion_rate)) + 
   geom_point(stat='identity', aes(col=for_against), size=7)  +
  ##scale_color_manual(name="Completion Rate", 
  ##                   labels = c("Above Average", "Below Average"), 
  ##                   values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Completion Rate", 
       subtitle="All time by team by season") + 
  ylim(0.65, 0.85) +
  coord_flip()



ggplot(plot_data5, 
        aes(x=for_against, y=completion_rate, label=completion_rate)) +
        geom_point(stat='identity', aes(col=completion_rate), size=6)  +
        geom_text(color="white", size=2) +
        labs(title="Completion Rate", 
             subtitle="All time by team by season") + 
        geom_smooth(method='lm',formula=y~x) 
        




# r2d3 test

r2d3(head(plot_data$completion_rate,10), script = "../barchart.js")

r2d3(data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20), script = "../barchart.js")








