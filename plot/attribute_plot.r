library(r2d3)
library(dplyr)

plot_data <- data.frame(paste(season_all_datamatrix$for_name
                        ,season_all_datamatrix$season,sep="-")
                        ,season_all_datamatrix$round
                        ,season_all_datamatrix$for_complete_sets
                        ,season_all_datamatrix$for_total_sets)


names(plot_data) <- c("team_season","round","for_completed_sets","for_total_sets")
head(plot_data,20)


completed_sets <- aggregate(plot_data$for_completed_sets, by=list(team_season=plot_data$team_season), FUN=sum)
total_sets <- aggregate(plot_data$for_total_sets, by=list(team_season=plot_data$team_season), FUN=sum)
plot_data2 <- data.frame(completed_sets,total_sets$x,completed_sets$x/total_sets$x)
names(plot_data2) <- c("team_season","completed_sets","total_sets","completion_rate")

head(plot_data2,20)
tail(plot_data2,20)





r2d3(head(plot_data$completion_rate,10), script = "../barchart.js")

r2d3(data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20), script = "../barchart.js")
