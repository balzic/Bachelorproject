source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\game.R')

## MSC GRAD STARTS
global_alpha <<- 0.3
resetMCData(RegularGradAscData)
rgrad_vs_random03 = playGames(500000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = playerRandom,p_switch = FALSE)
global_alpha <<- 0.2
resetMCData(RegularGradAscData)
rgrad_vs_random02 = playGames(500000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = playerRandom,p_switch = FALSE)
global_alpha <<- 0.1
resetMCData(RegularGradAscData)
rgrad_vs_random01 = playGames(500000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = playerRandom,p_switch = FALSE)
global_alpha <<- 0.5
resetMCData(RegularGradAscData)
rgrad_vs_random05 = playGames(500000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = playerRandom,p_switch = FALSE)
global_alpha <<- 0.4
resetMCData(RegularGradAscData)
rgrad_vs_random04 = playGames(500000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = playerRandom,p_switch = FALSE)

plot(1:(500000/100),rgrad_vs_random01$winrate,col="blue",ylim = c(0,1),xlim = c(0,5000),
     main = "500000 games - MC stochastic gradient ascent policy starts vs. Random policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")
points(1:(500000/100),rgrad_vs_random02$winrate,col="red")
points(1:(500000/100),rgrad_vs_random03$winrate,col="green")
points(1:(500000/100),rgrad_vs_random04$winrate,col="purple")
points(1:(500000/100),rgrad_vs_random05$winrate,col="brown")
legend("bottom",
       c("alpha = 0.1","alpha = 0.2","alpha = 0.3","alpha = 0.4", "alpha = 0.5"),
       fill=c("blue","red","green","purple","brown"))


## TEST OF OPTIMAL STRATEGY
global_alpha <<- 0
rgrad_vs_random = playGames(20000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = playerRandom,p_switch = FALSE)
plot(1:(20000/100),rgrad_vs_random$winrate,col="blue",ylim = c(0,1),xlim = c(0,200),
     main = "20000 games - trained MC stochastic gradient ascent policy starts vs. Random policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")

## RANDOM STARTS
global_alpha <<- 0.4
resetMCData(RegularGradAscData)
random_vs_rgrad04 = playGames(500000,second_player = newPlayer(RegularGradAscData,MCSGradAsc),first_player = playerRandom,p_switch = TRUE)

plot(1:(500000/100),random_vs_rgrad04$winrate,col="blue",ylim = c(0,1),xlim = c(0,5000),
     main = "500000 games - Random policy starts vs. MC stochastic gradient ascent policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")


## TEST OF OPTIMAL STRATEGY
global_alpha <<- 0.4
random_vs_rgrad04_t = playGames(20000,second_player = newPlayer(RegularGradAscData,MCSGradAsc),first_player = playerRandom,p_switch = TRUE)

plot(1:(20000/100),random_vs_rgrad04_t$winrate,col="blue",ylim = c(0,1),xlim = c(0,200),
     main = "20000 games - Random policy starts vs. trained MC stochastic gradient ascent policy",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")


