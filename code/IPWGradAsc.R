source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\game.R')
#source('C:\\Users\\hitma\\Dropbox\\Bachelorprojekt\\kode\\game.R')

playerLearn.ResetData()
IPWGD_vs_random = playGames(500000,first_player = newPlayer(NULL,IPWGradDesc),second_player = playerRandom,p_switch = FALSE)


plot(1:(500000/100),IPWGD_vs_random$winrate,col="blue",ylim = c(0,1),xlim = c(0,5000),
     main = "500000 games - Inverse probability weighted gradient ascent policy vs. Random policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")


playerLearn.ResetData()
IPWGD_vs_random_t = playGames(20000,first_player = newPlayer(NULL,IPWGradDesc),second_player = playerRandom,p_switch = FALSE)
playerLearn.ResetData()
IPWGD_vs_random_t04 = playGames(20000,first_player = newPlayer(NULL,IPWGradDesc),second_player = playerRandom,p_switch = FALSE)
plot(1:(20000/100),IPWGD_vs_random_t$winrate,col="blue",ylim = c(0,1),xlim = c(0,200),
     main = "20000 games - Trained inverse probability weighted gradient ascent policy vs. Random policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")
points(1:(20000/100),IPWGD_vs_random_t04$winrate)
