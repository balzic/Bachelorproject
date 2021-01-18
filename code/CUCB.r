source('~/Dropbox/Bachelorprojekt/kode/game.R')
options(device = "X11")

resetData(contextualUCBData)
resetData(contextualEpsData)
cucb_vs_random = playGames(100000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = playerRandom,p_switch = FALSE)
ceps_vs_random = playGames(100000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerRandom,p_switch = FALSE)
resetData(contextualUCBData)
resetData(contextualEpsData)
plot(1:(100000/100),cucb_vs_random$winrate,col="blue",ylim = c(0,1),xlim = c(0,1000),
     main = "40000 games - Contextual UCB policy vs. Contextual epsilon-greedy policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")
points(1:(100000/100),ceps_vs_random$winrate,col="red")
legend("bottom",
       c("Contextual UCB policy starts vs. Random policy","Contextual epsilon-greedy policy vs. Random policy"),
       fill=c("blue","red"))

global_epsilon <<- 0.01
resetData(contextualUCBData)
resetData(contextualEpsData)
cucb_vs_random2 = playGames(400000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = playerRandom,p_switch = FALSE)
ceps_vs_random2 = playGames(400000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerRandom,p_switch = FALSE)
resetData(contextualUCBData)
resetData(contextualEpsData)
plot(1:(400000/100),cucb_vs_random2$winrate,col="blue",ylim = c(0,1),xlim = c(0,4000),
     main = "400000 games - Contextual UCB policy vs. Contextual epsilon-greedy policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")
points(1:(400000/100),ceps_vs_random2$winrate,col="red")
legend("bottom",
       c("Contextual UCB policy starts vs. Random policy","Contextual epsilon-greedy policy starts vs. Random policy"),
       fill=c("blue","red"))

max(cucb_vs_random2$winrate)
max(ceps_vs_random2$winrate)
