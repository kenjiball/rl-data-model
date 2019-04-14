library(r2d3)
library(dplyr)
library(ggplot2)
library(devtools)
library(ggpmisc)

# Data preparation
plot_data <- season_all_datamatrix %>%
                    select(Home_Away, for_name,season,round,for_match_result 
                          ,for_complete_sets,for_total_sets,for_points
                          ,against_complete_sets,against_total_sets,against_points) 

plot_data$round <- as.numeric(plot_data$round)
names(plot_data)[2] <- "team"

# Prepare Season Win Totals for Aggregation
plot_data$competition_points <- gsub("Win",2,plot_data$for_match_result)
plot_data$competition_points <- gsub("Lose",0,plot_data$competition_points)
plot_data$competition_points <- gsub("Draw",1,plot_data$competition_points)
plot_data$competition_points <- (as.numeric(plot_data$competition_points))
names(plot_data$competition_points) <- "competition_points"

sapply(plot_data,class)
head(plot_data,10)

# Aggregate data by team by year
# Filter out Finals (keep just regular season)
plot_data2 <- plot_data %>% filter(round <= 26)

# Group by team and season to generate overall season metrics 
plot_data3 <- plot_data2 %>% group_by(team, season) %>%
                             summarise(completed_sets = sum(for_complete_sets, na.rm = TRUE)
                                      ,total_sets = sum(for_total_sets, na.rm = TRUE)
                                      ,competition_points = sum(competition_points, na.rm = TRUE)
                                      ,for_points = sum(for_points, na.rm = TRUE)
                                      ,against_points = sum(against_points, na.rm = TRUE))

# Mutate to include the completion rate
plot_data4 <- plot_data3 %>% mutate(completion_rate = completed_sets/total_sets
                                    , for_against = for_points/against_points
                                    , team_season = paste(team, season, sep = "_"))
plot_data4$completion_rate <- round(plot_data4$completion_rate,4)
plot_data4$for_against <- round(plot_data4$for_against,4)
plot_data5 <- plot_data4 %>% arrange(completion_rate)


# Generate data for win/draw/loss probability metrics
plot_data6 <- plot_data2 %>% mutate(for_completion_rate = for_complete_sets/for_total_sets
                                    , for_against = for_points - against_points
                                    , for_against_ratio = for_points/against_points
                                    , against_completion_rate = against_complete_sets/against_total_sets
                                    , for_against_completion_rate = for_completion_rate - against_completion_rate)
head(plot_data6,10)



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
        



ggplot(plot_data6, 
       aes(x=for_against, y=for_completion_rate, label=for_completion_rate)) +
  geom_point(stat='identity', aes(col=for_completion_rate), size=6)  +
  geom_text(color="white", size=2) +
  labs(title="Completion Rate", 
       subtitle="All time by team by season") + 
  geom_smooth(method='lm',formula=fit_formula) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
               formula = fit_formula, parse = TRUE)


# Density Plot
fit_formula <- y ~ poly(x, 1, raw = TRUE)

ggplot(plot_data6, 
       aes(x=for_against, y=for_completion_rate, label=for_completion_rate)) +
       geom_point(stat='identity', aes(col=for_completion_rate), size=6) +
       geom_density_2d() +
       geom_smooth(method='lm',formula=fit_formula) +
       stat_poly_eq(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
                   formula = fit_formula, parse = TRUE)

# Histogram of Attributes
ggplot(plot_data6,
       aes(x=for_against_completion_rate, label=for_against_completion_rate)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.02) +
      geom_density(alpha=.2, fill="#FF6666") +
      geom_vline( aes(xintercept=mean(for_against_completion_rate)), linetype="dashed") +
      facet_grid(for_match_result~Home_Away)


# r2d3 test

r2d3(head(plot_data$completion_rate,10), script = "../barchart.js")

r2d3(data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20), script = "../barchart.js")

# test
head(plot_data6 %>% arrange(completion_rate),5)

plot_data6 %>% filter(is.na(for_match_result))







