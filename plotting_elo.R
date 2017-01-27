library(ggplot2)
plot_table <- elo_table
plot_table$Team <- as.factor(plot_table$Team)

qplot(Date, ELO, data = plot_table, color = Team)