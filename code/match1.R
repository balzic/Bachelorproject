source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\game.R')
#source('C:\\Users\\hitma\\Dropbox\\Bachelorprojekt\\kode\\game.R')

IPWGD_vs_cucb = playGames(80000,first_player = newPlayer(NULL,IPWGradDesc),second_player = newPlayer(contextualUCBData,contextualUCBAction),p_switch = FALSE)
playerLearn.ResetData()
resetData(contextualUCBData)
cucb_vs_IPWGD = playGames(80000,second_player = newPlayer(NULL,IPWGradDesc),first_player = newPlayer(contextualUCBData,contextualUCBAction),p_switch = FALSE)

plot(1:(80000/100),IPWGD_vs_cucb$winrate,col="blue",ylim = c(0,1),xlim = c(0,800),
     main = "80000 games - IPW gradient descent policy vs. Contextual UCB policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")
points(1:(80000/100),cucb_vs_IPWGD$winrate,col="red")
legend("bottom",
       c("Winrate of IPW gradient descent policy where it plays first","Winrate of contextual UCB policy where it plays first"),
       fill=c("blue","red"))


max(IPWGD_vs_cucb$wins) - max(IPWGD_vs_cucb$losses) 
max(IPWGD_vs_cucb$draws)


max(cucb_vs_IPWGD$losses) - max(cucb_vs_IPWGD$wins)
max(cucb_vs_IPWGD$draws)
