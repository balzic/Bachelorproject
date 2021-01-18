#source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\game.R')
source('C:\\Users\\hitma\\Dropbox\\Bachelorprojekt\\kode\\game.R')

global_epsilon <<- 0.01
resetMCData(MCEpsSoftData)
mceps_vs_random = playGames(500000,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),second_player = playerRandom,p_switch = FALSE)
resetMCData(MCEpsSoftData2)
random_vs_mceps = playGames(500000,first_player = playerRandom,second_player = newPlayer(MCEpsSoftData2,MCEpsSoftAction),p_switch = TRUE)
plot(1:(500000/100),mceps_vs_random$winrate,col="blue",ylim = c(0,1),xlim = c(0,5000),
     main = "500000 games - Monte Carlo epsilon-greedy policy starts vs. Random policy",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")
points(1:(500000/100),random_vs_mceps$winrate,col="red")
legend("bottom",
       c("Trained Monte Carlo epsilon-greedy policy plays first","Trained Monte Carlo epsilon-greedy policy plays second"),
       fill=c("blue","red"))
max(mceps_vs_random$winrate)

global_epsilon <<- 0.00
mceps_vs_random2 = playGames(20000,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),second_player = playerRandom,p_switch = FALSE)
random_vs_mceps2 = playGames(20000,first_player = playerRandom,second_player = newPlayer(MCEpsSoftData2,MCEpsSoftAction),p_switch = TRUE)
plot(1:(20000/100),mceps_vs_random2$winrate,col="blue",ylim = c(0,1),xlim = c(0,200),
     main = "20000 games - Trained Monte Carlo epsilon-greedy policy vs. Random policy",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")
points(1:(20000/100),random_vs_mceps2$winrate,col="red")
legend("bottom",
       c("Trained Monte Carlo epsilon-greedy policy plays first","Trained Monte Carlo epsilon-greedy policy plays second"),
       fill=c("blue","red"))

global_epsilon <<- 0.05
resetMCData(MCEpsSoftData)
resetMCData(MCEpsSoftData2) ## Remember to consider the constant, try out some more tomorrow. with 1000000+ games
mceps_vs_mceps = playGames(50000,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),second_player = newPlayer(MCEpsSoftData2,MCEpsSoftAction),p_switch = FALSE)
plot(1:(50000/100),mceps_vs_mceps$winrate,col="blue",ylim = c(0,1),xlim = c(0,500),
     main = "500000 games - On-policy First-visit Monte Carlo policy vs. itself",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")
points(1:(50000/100),random_vs_mceps$winrate,col="red")
#legend("bottom",
#       c("On-policy First-visit Monte Carlo policy plays first","On-policy First-visit Monte Carlo policy plays second"),
#       fill=c("blue","red"))
#max(mceps_vs_random$winrate)


global_epsilon <<- 0.05
resetMCData(MCEpsSoftData)
resetMCData(MCEpsSoftData2)
mceps_vs_random = playGames(40000,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),second_player = playerRandom,p_switch = FALSE)
mceps_vs_random2 = playGames(40000,first_player = newPlayer(MCEpsSoftData2,MCEpsSoftAction),second_player = playerRandom,p_switch = FALSE)
plot(1:(40000/100),mceps_vs_random$winrate,col="blue",ylim = c(0,1),xlim = c(0,400),
     main = "40000 games - Comparison of on-policy First-visit Monte Carlo policy constants",
     xlab = "Number of games divided by 100",
     ylab = "Winrate")
points(1:(40000/100),mceps_vs_random2$winrate,col="red")
legend("bottom",
       c("alpha = 1/5","alpha = 1/T(n)"),
       fill=c("blue","red"))


global_alpha <<- 2
resetMCData(RegularGradAscData)
mcsgrad_vs_random2 = playGames(200000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = playerRandom,p_switch = FALSE)
global_epsilon <<- 0.001
resetMCData(MCEpsSoftData)
mceps_vs_random2 = playGames(200000,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),second_player = playerRandom,p_switch = FALSE)
plot(1:(200000/100),mceps_vs_random$winrate,col="blue",ylim = c(0,1),xlim = c(0,2000),
     main = "200000 games - Monte Carlo policies start vs. Random policy",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")
points(1:(200000/100),mcsgrad_vs_random2$winrate,col="red")
legend("bottom",
       c("Monte Carlo epsilon-greedy with epsilon = 0.001","Monte Carlo stochastic gradient ascent with alpha = 2"),
       fill=c("blue","red"))



global_epsilon <<- 0.01
resetMCData(MCEpsSoftData)
resetMCData(RegularGradAscData)
mceps_vs_random = playGames(200000,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),second_player = playerRandom,p_switch = FALSE)
resetMCData(MCEpsSoftData)
resetMCData(RegularGradAscData)
mcsgrad_vs_random = playGames(200000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = playerRandom,p_switch = FALSE)
plot(1:(200000/100),mceps_vs_random$winrate,col="blue",ylim = c(0,1),xlim = c(0,2000),
     main = "200000 games - Monte Carlo policies start vs. Random policy",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")
points(1:(200000/100),mcsgrad_vs_random$winrate,col="red")
legend("bottom",
       c("Monte Carlo epsilon-greedy with epsilon = 0.01","Monte Carlo stochastic gradient ascent with alpha = 0.4"),
       fill=c("blue","red"))
max(mceps_vs_random$winrate)
