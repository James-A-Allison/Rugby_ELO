
# Qualifing ---------------------------------------------------------------


# Africa ------------------------------------------------------------------


Africa_Gold_cup_2017 <- sim_tournament(readWorkbook("Fixtures.xlsx", "Africa_Gold_Cup_2017")) # loser relageted, Morocco enters
Africa_Gold_cup_2018 <- sim_tournament(fixtures = generate_fixtures(teams = c(as.character(Africa_Gold_cup_2017$Team[1:5]), "Morocco"), times_vs_opp = 1)) # winner becomes Africa_1, runner_up goes to Africa_reperchae
Africa_1 <- Africa_Gold_cup_2018$Team[1]
Africa_runner_up <- Africa_Gold_cup_2018[Africa_Gold_cup_2018 != "Morocco"][2]
Africa_repechage <- sim_game(teams = c(Africa_runner_up, "Morocco")) # winner enters repechage


# Repechage ---------------------------------------------------------------


# World Cup ---------------------------------------------------------------



Pool_A <- c("Ireland", "Scotland", "Japan", Europe_1, Playoff_winner)
Pool_B <- c("New Zealand", "South Africa", "Italy", Africa_1, Repecharge)
Pool_C <- c("England", "France", "Argentina", "United States of America", Oceana_2)
Pool_D <- c("Australia", "Wales", "Georgia", "Fiji", Americas_2)

QF1 <- c(Pool_A_winner, Pool_B_runner_up)
QF2 <- c(Pool_D_winner, Pool_C_runner_up)
QF3 <- c(Pool_B_winner, Pool_A_runner_up)
QF4 <- c(Pool_C_winner, Pool_D_runner_up)

SF1 <- c(QF1_winner, QF2_winner)
SF2 <- c(QF3_winner, QF4_winner)

bronze <- c(SF1_loser, SF2_loser)
WCF <- c(SF1_winner, SF2_winner)
