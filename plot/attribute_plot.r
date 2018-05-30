library(r2d3)
library(dplyr)
library(ggplot2)


# Data preparation
plot_data <- data.frame(paste(season_all_datamatrix$for_name
                        ,season_all_datamatrix$season,sep="-")
                        ,season_all_datamatrix$round
                        ,season_all_datamatrix$for_complete_sets
                        ,season_all_datamatrix$for_total_sets)
names(plot_data) <- c("team_season","round","for_completed_sets","for_total_sets")

completed_sets <- aggregate(plot_data$for_completed_sets, by=list(team_season=plot_data$team_season), FUN=sum)
total_sets <- aggregate(plot_data$for_total_sets, by=list(team_season=plot_data$team_season), FUN=sum)
plot_data2 <- data.frame(completed_sets,total_sets$x,round(completed_sets$x/total_sets$x,3))
names(plot_data2) <- c("team_season","completed_sets","total_sets","completion_rate")
plot_data3 <- plot_data2 %>% arrange(completion_rate)

head(plot_data3,20)
tail(plot_data3,20)
class(plot_data3$team_season)

# Plot
theme_set(theme_bw())

ggplot(plot_data3, aes(x=reorder(team_season,completion_rate,sum), y=completion_rate, label=completion_rate)) + 
   geom_point(stat='identity', aes(col=completion_rate), size=7)  +
  ##scale_color_manual(name="Completion Rate", 
  ##                   labels = c("Above Average", "Below Average"), 
  ##                   values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Completion Rate", 
       subtitle="All time by team by season") + 
  ylim(0.65, 0.85) +
  coord_flip()



r2d3(head(plot_data$completion_rate,10), script = "../barchart.js")

r2d3(data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20), script = "../barchart.js")








