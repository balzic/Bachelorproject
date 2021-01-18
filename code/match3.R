source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\game.R')
#source('C:\\Users\\hitma\\Dropbox\\Bachelorprojekt\\kode\\game.R')



ipwg_vs_eps = playGames(80000,first_player = newPlayer(NULL,IPWGradDesc),second_player = newPlayer(epsilonData,epsGreedyAction),p_switch = FALSE)
playerLearn.ResetData()
eps_vs_ipwg = playGames(80000,second_player = newPlayer(NULL,IPWGradDesc),first_player = newPlayer(epsilonData,epsGreedyAction),p_switch = FALSE)
eps_vs_ipwg
plot(1:(80000/100),ipwg_vs_eps$winrate,col="blue",ylim = c(0,1),xlim = c(0,800),
     main = "80000 games - IPW gradient ascent policy vs. non-contextual epsilon-greedy policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")
points(1:(80000/100),eps_vs_ipwg$winrate,col="red")
legend("bottom",
       c("Winrate of the IPW gradient ascent policy where it plays first","Winrate of the non-contextual epsilon-greedy policy where it plays first"),
       fill=c("blue","red"))



max(ipwg_vs_eps$wins)
max(ipwg_vs_eps$losses)
max(ipwg_vs_eps$draws)
max(ipwg_vs_eps$wins) - max(ipwg_vs_eps$losses)


max(eps_vs_ipwg$wins)
max(eps_vs_ipwg$losses)
max(eps_vs_ipwg$draws)
max(eps_vs_ipwg$losses) - max(eps_vs_ipwg$wins)
