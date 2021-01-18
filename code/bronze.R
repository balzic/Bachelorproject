source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\game.R')
#source('C:\\Users\\hitma\\Dropbox\\Bachelorprojekt\\kode\\game.R')


resetMCData(MCEpsSoftData)
playerLearn.ResetData()
mceps_vs_ipwga = playGames(80000,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),second_player = newPlayer(NULL,IPWGradDesc),p_switch = FALSE)
resetMCData(MCEpsSoftData)
playerLearn.ResetData()
ipwga_vs_mceps = playGames(80000,second_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),first_player = newPlayer(NULL,IPWGradDesc),p_switch = FALSE)

plot(1:(80000/100),mceps_vs_ipwga$winrate,col="blue",ylim = c(0,1),xlim = c(0,800),
     main = "80000 games - Monte Carlo epsilon-greedy policy vs. IPW gradient ascent policy",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")
points(1:(80000/100),ipwga_vs_mceps$winrate,col="red")
legend("bottom",
       c("Win rate of the Monte Carlo epsilon-greedy policy where it plays first","Win rate of the IPW gradient ascent policy where it plays first"),
       fill=c("blue","red"))



max(mceps_vs_ipwga$wins)
max(mceps_vs_ipwga$losses)
max(mceps_vs_ipwga$draws)
max(mceps_vs_ipwga$wins) - max(mceps_vs_ipwga$losses)


max(ipwga_vs_mceps$wins)
max(ipwga_vs_mceps$losses)
max(ipwga_vs_mceps$draws)
max(ipwga_vs_mceps$losses) - max(ipwga_vs_mceps$wins)
