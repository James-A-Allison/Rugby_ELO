library(ggplot2)

plot <- ggplot(data = elo_table[elo_table$Team == "Italy" | elo_table$Team == "Six Nations",], aes(x = Date, y = ELO, color = Team))
plot <- plot + geom_step(show.legend = T)
plot