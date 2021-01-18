source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\game.R')
#source('C:\\Users\\hitma\\Dropbox\\Bachelorprojekt\\kode\\game.R')


global_alpha = 0.4
resetMCData(RegularGradAscData)
playerLearn.ResetData()
MCSGradAsc_vs_ipwga = playGames(80000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = newPlayer(NULL,IPWGradDesc),p_switch = FALSE)
playerLearn.ResetData()
resetMCData(RegularGradAscData)
ipwga_vs_MCSGradAsc = playGames(80000,second_player = newPlayer(RegularGradAscData,MCSGradAsc),first_player = newPlayer(NULL,IPWGradDesc),p_switch = FALSE)

plot(1:(80000/100),MCSGradAsc_vs_ipwga$winrate,col="blue",ylim = c(0,1),xlim = c(0,800),
     main = "80000 games - Monte Carlo gradient ascent policy vs. IPW gradient ascent policy",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")
points(1:(80000/100),ipwga_vs_MCSGradAsc$winrate,col="red")
legend("bottom",
       c("Win rate of the Monte Carlo gradient ascent policy where it plays first","Win rate of the IPW gradient ascent policy where it plays first"),
       fill=c("blue","red"))



max(MCSGradAsc_vs_ipwga$wins)
max(MCSGradAsc_vs_ipwga$losses)
max(MCSGradAsc_vs_ipwga$draws)
max(MCSGradAsc_vs_ipwga$wins) - max(MCSGradAsc_vs_ipwga$losses)


max(ipwga_vs_MCSGradAsc$wins)
max(ipwga_vs_MCSGradAsc$losses)
max(ipwga_vs_MCSGradAsc$draws)
max(ipwga_vs_MCSGradAsc$losses) - max(ipwga_vs_MCSGradAsc$wins)
