team_A <- "New Zealand"
team_B <- "England"

elo_team_A <- current_ratings(teams_in = team_A)[,2]
elo_team_B <- current_ratings(teams_in = team_B)[,2]

expected_score_home <- 1/(1+10^((elo_team_B-elo_team_A)/400))
expected_score_away <- 1/(1+10^((elo_team_A-elo_team_B)/400))

sample.space <- c(team_A,team_B)
theta <- expected_score_home
N <- 1

flips <- sample(sample.space,
                size = N,
                replace = TRUE,
                prob = c(theta, 1-theta))
flips

fixtures <- readWorkbook("Fixtures.xlsx", sheet = "Rugby_Championship_2017")
fixtures$Date <- as.Date(fixtures$Date, origin = "1900-01-01") - 2

simulation_output <- c()

for (j in 1:10000){
rugby_champtionship_table <- data.frame(Team = as.character(unique(fixtures$Team_A)),
                                        Points = as.numeric(rep(0,length(unique(fixtures$Team_A)))))

for (i in 1:length(fixtures[,1])) {
  match <- fixtures[i,]
  team_A <- match$Team_A
  team_B <- match$Team_B
  
  elo_team_A <- current_ratings(teams_in = team_A)[,2]
  elo_team_B <- current_ratings(teams_in = team_B)[,2]
  
  expected_score_home <- 1/(1+10^((elo_team_B-elo_team_A)/400))
  expected_score_away <- 1/(1+10^((elo_team_A-elo_team_B)/400))
 
  winner <- sample(c(team_A, team_B),
                  size = 1,
                  replace = TRUE,
                  prob = c(expected_score_home, 1-theta))
  if (winner == team_A) {
    rugby_champtionship_table$Points[rugby_champtionship_table$Team == team_A] <- rugby_champtionship_table$Points[rugby_champtionship_table$Team == team_A] + 3
  } else {
    rugby_champtionship_table$Points[rugby_champtionship_table$Team == team_B] <- rugby_champtionship_table$Points[rugby_champtionship_table$Team == team_B] + 3
  }
}
rugby_champtionship_table <- rugby_champtionship_table[order(rugby_champtionship_table$Points, decreasing = T),]
rugby_champtionship_table$Position <- order(rugby_champtionship_table$Points, decreasing = T)
rugby_champtionship_table$simulation_no <- j
simulation_output <- rbind(simulation_output, rugby_champtionship_table)}

sim_grouped <- aggregate(simulation_output$Position, by=list(simulation_output$Team, simulation_output$Position), FUN = length)
colnames(sim_grouped) <- c("Team", "Position", "Chance")
sim_grouped$Chance <- sim_grouped$Chance / 10000

plot_ob <- ggplot(data = sim_grouped, mapping = aes(x = Position, y = Chance, fill = Team))
plot_ob <- plot_ob + geom_col()
plot_ob <- plot_ob + scale_fill_manual(values = c("Sky Blue", "Gold", "Black", "Dark Green"))
plot_ob


af_gold_cup_17_fx <- readWorkbook("Fixtures.xlsx", "Africa_Gold_Cup_2017")


