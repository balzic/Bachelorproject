source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\game.R')
#source('C:\\Users\\hitma\\Dropbox\\Bachelorprojekt\\kode\\game.R')



resetMCData(MCEpsSoftData)
resetMCData(RegularGradAscData)
MCSGradAsc_vs_mceps = playGames(80000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),p_switch = FALSE)
resetMCData(MCEpsSoftData)
resetMCData(RegularGradAscData)
mceps_vs_MCSGradAsc = playGames(80000,second_player = newPlayer(RegularGradAscData,MCSGradAsc),first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),p_switch = FALSE)

plot(1:(80000/100),MCSGradAsc_vs_mceps$winrate,col="blue",ylim = c(0,1),xlim = c(0,800),
     main = "80000 games - Monte Carlo gradient ascent policy vs. Monte Carlo epsilon-greedy policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")
points(1:(80000/100),mceps_vs_MCSGradAsc$winrate,col="red")
legend("bottom",
       c("Winrate of the Monte Carlo gradient ascent policy where it plays first","Winrate of the Monte Carlo epsilon-greedy policy where it plays first"),
       fill=c("blue","red"))



max(MCSGradAsc_vs_mceps$wins)
max(MCSGradAsc_vs_mceps$losses)
max(MCSGradAsc_vs_mceps$draws)
max(MCSGradAsc_vs_mceps$wins) - max(MCSGradAsc_vs_mceps$losses)


max(mceps_vs_MCSGradAsc$wins)
max(mceps_vs_MCSGradAsc$losses)
max(mceps_vs_MCSGradAsc$draws)
max(mceps_vs_MCSGradAsc$losses) - max(mceps_vs_MCSGradAsc$wins)
