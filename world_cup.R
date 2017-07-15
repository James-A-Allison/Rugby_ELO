simulation_out <- data.frame(Team = as.character(),
                              Progress = as.character())
all_teams <- unique(elo_table$Team)
for (i in 1:10000){
# Africa qualifying -------------------------------------------------------

Africa_Gold_cup_2017 <- sim_tournament(readWorkbook("Fixtures.xlsx", "Africa_Gold_Cup_2017")) # loser relageted, Morocco enters
Africa_Gold_cup_2018 <- sim_tournament(fixtures = generate_fixtures(teams = c(as.character(Africa_Gold_cup_2017$Team[1:5]), "Morocco"), times_vs_opp = 1)) # winner becomes Africa_1, runner_up goes to Africa_reperchae
Africa_1 <- Africa_Gold_cup_2018$Team[1]
Africa_runner_up <- Africa_Gold_cup_2018$Team[Africa_Gold_cup_2018$Team != "Morocco"][2]
Africa_repechage <- sim_game(c(Africa_runner_up, "Morocco")) # winner enters repechage


# Europe qualifying -------------------------------------------------------

Round_four_final_winner <- sim_game(c("Portugal", "Czech Republic"))
Rugby_Europe_Championship <- sim_tournament(fixtures = generate_fixtures(teams = c("Romania", "Spain", "Russia", "Germany", "Belgium"), times_vs_opp = 1))
Europe_1 <- Rugby_Europe_Championship$Team[1]
Rugby_Europe_Championship_runner_up <- Rugby_Europe_Championship$Team[2]
Europe_2 <- sim_game(c(Rugby_Europe_Championship_runner_up, Round_four_final_winner))
# Asia qualifying ---------------------------------------------------------
Asian_Rugby_Championship <- sim_tournament(generate_fixtures(teams = c("Hong Kong", "Malaysia", "South Korea"), times_vs_opp = 2))
Asian_Rugby_Championship_winner <- Asian_Rugby_Championship$Team[1]

# Americas qualifiying ----------------------------------------------------

Americas_2 <- sim_game(c("Canada", "Uruguay"))
Americas_repechage <- c("Canada", "Uruguay")[!(c("Canada", "Uruguay") %in% Americas_2)]

# Oceania qualifiying -----------------------------------------------------

Oceania_Rugby_World_Cup_winner <- sim_game(c("Cook Islands", "Tahiti"))
Oceania_2 <- sim_game(c("Tonga", "Samoa"))
Oceania_3 <- c("Tonga", "Samoa")[!(c("Tonga", "Samoa") %in% Oceania_2)]

# Repechage ---------------------------------------------------------------

Playoff_winner <- sim_game(c(Oceania_3,Europe_2))
euro_oceania <- c(Oceania_3,Europe_2)[!(c(Oceania_3,Europe_2) %in% Playoff_winner)]

oceania_asia <- sim_game(c(Oceania_Rugby_World_Cup_winner,Asian_Rugby_Championship_winner))

Repecharge <- sim_tournament(generate_fixtures(teams = c(euro_oceania, oceania_asia,Africa_repechage,Americas_repechage), times_vs_opp = 1))
Repecharge <- Repecharge$Team[1]

# World Cup ---------------------------------------------------------------



Pool_A <- sim_tournament(fixtures = generate_fixtures(teams = c("Ireland", "Scotland", "Japan", Europe_1, Playoff_winner), times_vs_opp = 1))
Pool_B <- sim_tournament(fixtures = generate_fixtures(teams = c("New Zealand", "South Africa", "Italy", Africa_1, Repecharge), times_vs_opp = 1))
Pool_C <- sim_tournament(fixtures = generate_fixtures(teams = c("England", "France", "Argentina", "United States of America", Oceania_2), times_vs_opp = 1))
Pool_D <- sim_tournament(fixtures = generate_fixtures(teams = c("Australia", "Wales", "Georgia", "Fiji", Americas_2), times_vs_opp = 1))

Pool_A_winner <- Pool_A$Team[1]
Pool_A_runner_up <- Pool_A$Team[2]
Pool_A_auto_qualifier <- Pool_A$Team[3]

Pool_B_winner <- Pool_B$Team[1]
Pool_B_runner_up <- Pool_B$Team[2]
Pool_B_auto_qualifier <- Pool_B$Team[3]

Pool_C_winner <- Pool_C$Team[1]
Pool_C_runner_up <- Pool_C$Team[2]
Pool_C_auto_qualifier <- Pool_C$Team[3]

Pool_D_winner <- Pool_D$Team[1]
Pool_D_runner_up <- Pool_D$Team[2]
Pool_D_auto_qualifier <- Pool_D$Team[3]


QF1 <- sim_game(c(Pool_A_winner, Pool_B_runner_up))
QF2 <- sim_game(c(Pool_D_winner, Pool_C_runner_up))
QF3 <- sim_game(c(Pool_B_winner, Pool_A_runner_up))
QF4 <- sim_game(c(Pool_C_winner, Pool_D_runner_up))

SF1 <- sim_game(c(QF1, QF2))
SF2 <- sim_game(c(QF3, QF4))

bronze <- sim_game(c(c(QF1, QF2)[!(c(QF1, QF2) %in% SF1)], c(QF3, QF4)[!(c(QF3, QF4) %in% SF2)]))
WC <- sim_game(c(SF1, SF2))

output_progress <- data.frame(Team = as.character(),
                              Progress = as.character())
new_row <- data.frame(Team = as.character(WC),
                      Progress = as.character("Champions"))
output_progress <- rbind(output_progress, new_row)
new_row <- data.frame(Team = as.character(c(SF1, SF2)[!(c(SF1, SF2) %in% WC)]),
                      Progress = as.character("Runner up"))
output_progress <- rbind(output_progress, new_row)
new_row <- data.frame(Team = as.character(bronze),
                      Progress = as.character("Bronze"))
output_progress <- rbind(output_progress, new_row)
new_row <- data.frame(Team = as.character(c(QF1, QF2, QF3, QF4)[!(c(QF1, QF2, QF3, QF4) %in% output_progress$Team)]),
                      Progress = as.character("Fourth place"))
output_progress <- rbind(output_progress, new_row)
new_row <- data.frame(Team = as.character(c(Pool_A_winner, Pool_B_runner_up, 
                                            Pool_D_winner, Pool_C_runner_up,
                                            Pool_B_winner, Pool_A_runner_up,
                                            Pool_C_winner, Pool_D_runner_up)[!(c(Pool_A_winner, Pool_B_runner_up, 
                                                                                 Pool_D_winner, Pool_C_runner_up,
                                                                                 Pool_B_winner, Pool_A_runner_up,
                                                                                 Pool_C_winner, Pool_D_runner_up) %in% output_progress$Team)]),
                      Progress = as.character("Quarter finals"))
output_progress <- rbind(output_progress, new_row)
new_row <- data.frame(Team = as.character(c(Pool_A_auto_qualifier, Pool_B_auto_qualifier, 
                                            Pool_C_auto_qualifier, Pool_D_auto_qualifier
                                            )[!(c(Pool_A_auto_qualifier, Pool_B_auto_qualifier, 
                                                  Pool_C_auto_qualifier, Pool_D_auto_qualifier) %in% output_progress$Team)]),
                      Progress = as.character("Automatic qualifiation for RWC 2021"))
output_progress <- rbind(output_progress, new_row)
new_row <- data.frame(Team = as.character(c(Pool_A$Team, Pool_B$Team, 
                                            Pool_C$Team, Pool_D$Team)[!(c(Pool_A$Team, Pool_B$Team, 
                                            Pool_C$Team, Pool_D$Team) %in% output_progress$Team)]),
                      Progress = as.character("Group stage"))
output_progress <- rbind(output_progress, new_row)
new_row <- data.frame(Team = as.character(all_teams[!(all_teams %in% output_progress$Team)]),
                      Progress = as.character("Did not qualify"))
output_progress <- rbind(output_progress, new_row)
simulation_out <- rbind(simulation_out, output_progress)
print(i)
}

sim_grouped <- aggregate(simulation_out$Progress, by=list(simulation_out$Team, simulation_out$Progress), FUN = length)
colnames(sim_grouped) <- c("Team", "Progress", "Chance")
sim_grouped$Chance <- sim_grouped$Chance / i * 100
sim_grouped <- sim_grouped[!(sim_grouped$Progress == "Did not qualify" & sim_grouped$Chance == 100),]

plot_ob <- ggplot(data = sim_grouped, mapping = aes(x = Team, y = Chance, fill = Progress))
plot_ob <- plot_ob + geom_col()
plot_ob <- 
# plot_ob <- plot_ob + scale_fill_manual(values = c("Sky Blue", "Gold", "Black", "Dark Green"))
plot_ob

write.csv(sim_grouped, "sim_grouped.csv", row.names = F)
