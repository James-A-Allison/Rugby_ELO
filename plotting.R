plot_table <- elo_table
plot_table$Team <- as.factor(plot_table$Team)

# qplot(Date, ELO, data = plot_table, color = Team, geom = "line")

qplot(Date, ELO, data = plot_table[plot_table$Team == "Georgia" | plot_table$Team == "Italy",], color = Team, geom = "step")

new_six_nations <- plot_table[((plot_table$Team == "New Zealand" | plot_table$Team == "Wales" |
                                  plot_table$Team == "Ireland" | plot_table$Team == "France" |
                                  plot_table$Team == "Scotland" | plot_table$Team == "Italy") &
                                 plot_table$Date > "1980-01-01"),]

qplot(Date, ELO, data = new_six_nations, color = Team, geom = "step")

new_six_nations <- plot_table[(plot_table$Team %in% classifications$Six_Nations | plot_table$Team %in% classifications$Tri_Nations | plot_table$Team == "Georgia") &
                                plot_table$Date > "2000-01-01",]

qplot(Date, ELO, data = new_six_nations, color = Team, geom = "step")

chartFile <- read.csv("~/Documents/Rugby_ELO/elo_table.csv", stringsAsFactors = F)
teams <- unique(chartFile$Team)[1:5]
plot_data <- chartFile[chartFile$Team %in% teams,]
plot_data$Date <- as.Date(plot_data$Date)
plot_object <- ggplot(data = plot_data)
plot_object <- plot_object + geom_line(aes(x = Date, y = ELO, color = Team))
plot_object