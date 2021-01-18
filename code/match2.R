source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\game.R')
#source('C:\\Users\\hitma\\Dropbox\\Bachelorprojekt\\kode\\game.R')


resetData(contextualEpsData)
resetMCData(MCEpsSoftData)
mceps_vs_ceps = playGames(80000,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = FALSE)
resetData(contextualEpsData)
resetMCData(MCEpsSoftData)
ceps_vs_mceps = playGames(80000,second_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = FALSE)

plot(1:(80000/100),mceps_vs_ceps$winrate,col="blue",ylim = c(0,1),xlim = c(0,800),
     main = "80000 games - Monte Carlo epsilon-greedy policy vs. Contextual epsilon-greedy policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")
points(1:(80000/100),ceps_vs_mceps$winrate,col="red")
legend("bottom",
       c("Winrate of the Monte Carlo epsilon-greedy policy where it plays first","Winrate of the contextual epsilon-greedy policy where it plays first"),
       fill=c("blue","red"))



max(mceps_vs_ceps$wins)
max(mceps_vs_ceps$losses)
max(mceps_vs_ceps$draws)
max(mceps_vs_ceps$wins) - max(mceps_vs_ceps$losses)


max(ceps_vs_mceps$wins)
max(ceps_vs_mceps$losses)
max(ceps_vs_mceps$draws)
max(ceps_vs_mceps$losses) - max(ceps_vs_mceps$wins)
