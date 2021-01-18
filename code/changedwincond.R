#source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\game.R')
source('C:\\Users\\hitma\\Dropbox\\Bachelorprojekt\\kode\\game.R')
## change winning conditions
ChangeWinningConditions <<- TRUE
radical <<- TRUE
global_alpha <<- 0.4
resetMCData(RegularGradAscData)
mcsgradasc_vs_random_cwc = playGames(100000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = playerRandom,p_switch = FALSE)

plot(1:(100000/100),mcsgradasc_vs_random_cwc$winrate,col="blue",ylim = c(0,1),xlim = c(0,1000),
     main = "100000 games - Monte Carlo gradient ascent starts vs. MC stochastic gradient ascent policy",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")
RegularGradAscData$return$`0, 0, 0, 0, 0, 0, 0, 0, 0`
RegularGradAscData$return$`-1, 0, 0, 0, 1, 0, 0, 0, 0`
RegularGradAscData$return$`-1, -1, 1, 0, 1, 0, 0, 0, 1`

ChangeWinningConditions <<- TRUE
playerLearn.ResetData()
ipwga_vs_random_cwc = playGames(100000,first_player = newPlayer(NULL,IPWGradDesc),second_player = playerRandom,p_switch = FALSE)

plot(1:(100000/100),ipwga_vs_random_cwc$winrate,col="blue",ylim = c(0,1),xlim = c(0,1000),
     main = "100000 games - IPW gradient ascent vs. Random policy",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")

ChangeWinningConditions <<- TRUE
resetMCData(MCEpsSoftData)
mceps_vs_random_cwc = playGames(100000,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),second_player = playerRandom,p_switch = FALSE)

plot(1:(100000/100),mceps_vs_random_cwc$winrate,col="blue",ylim = c(0,1),xlim = c(0,1000),
     main = "100000 games - Random policy starts vs. MC stochastic gradient ascent policy",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")


ChangeWinningConditions <<- TRUE
eps_vs_random_cwc = playGames(100000,first_player = newPlayer(epsilonData,epsGreedyAction),second_player = playerRandom,p_switch = FALSE)

plot(1:(100000/100),eps_vs_random_cwc$winrate,col="blue",ylim = c(0,1),xlim = c(0,1000),
     main = "100000 games - Epsilon-greedy starts vs. MC stochastic gradient ascent policy",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")



ChangeWinningConditions <<- TRUE
resetData(contextualUCBData)
cucb_vs_random_cwc = playGames(100000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = playerRandom,p_switch = FALSE)
ChangeWinningConditions <<- TRUE
resetData(contextualEpsData)
ceps_vs_random_cwc = playGames(100000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerRandom,p_switch = FALSE)
ChangeWinningConditions <<- TRUE
eps_vs_random_cwc = playGames(100000,first_player = newPlayer(epsilonData,epsGreedyAction),second_player = playerRandom,p_switch = FALSE)
ChangeWinningConditions <<- TRUE
resetMCData(MCEpsSoftData)
mceps_vs_random_cwc = playGames(100000,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),second_player = playerRandom,p_switch = FALSE)
ChangeWinningConditions <<- TRUE
playerLearn.ResetData()
ipwga_vs_random_cwc = playGames(100000,first_player = newPlayer(NULL,IPWGradDesc),second_player = playerRandom,p_switch = FALSE)
ChangeWinningConditions <<- TRUE
global_alpha <<- 0.4
resetMCData(RegularGradAscData)
mcsgradasc_vs_random_cwc = playGames(100000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = playerRandom,p_switch = FALSE)



plot(1:(100000/100),mcsgradasc_vs_random_cwc$winrate,col="blue",ylim = c(0,1),xlim = c(0,1000),
     main = "100000 games - Changing win conditions for every 30000th game",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")
points(1:(100000/100),ipwga_vs_random_cwc$winrate,col="red")
points(1:(100000/100),mceps_vs_random_cwc$winrate,col="green")
points(1:(100000/100),eps_vs_random_cwc$winrate,col="brown")
points(1:(100000/100),cucb_vs_random_cwc$winrate,col="gray")
points(1:(100000/100),ceps_vs_random_cwc$winrate,col="purple")
legend("bottom",
       c("Monte Carlo gradient ascent starts policy starts vs. Random policy",
         "IPW gradient ascent policy starts vs. Random policy",
         "Monte Carlo epsilon-greedy policy starts vs. Random policy",
         "Epsilon-greedy policy starts vs. Random policy",
         "Contextual UCB policy starts vs. Random policy", 
         "Contextual epsilon-greedy starts vs. Random policy"),
       fill=c("blue","red","green","brown","gray","purple"))


ChangeWinningConditions <<- TRUE
resetData(contextualUCBData)
random_vs_cucb_cwc = playGames(100000,second_player = newPlayer(contextualUCBData,contextualUCBAction),first_player = playerRandom,p_switch = TRUE)
ChangeWinningConditions <<- TRUE
resetData(contextualEpsData)
random_vs_ceps_cwc = playGames(100000,second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),first_player = playerRandom,p_switch = TRUE)
ChangeWinningConditions <<- TRUE
random_vs_eps_cwc = playGames(100000,second_player = newPlayer(epsilonData,epsGreedyAction),first_player = playerRandom,p_switch = TRUE)
ChangeWinningConditions <<- TRUE
resetMCData(MCEpsSoftData)
random_vs_mceps_cwc = playGames(100000,second_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),first_player = playerRandom,p_switch = TRUE)
ChangeWinningConditions <<- TRUE
playerLearn.ResetData()
random_vs_ipwga_cwc = playGames(100000,second_player = newPlayer(NULL,IPWGradDesc),first_player = playerRandom,p_switch = TRUE)
ChangeWinningConditions <<- TRUE
global_alpha <<- 0.4
resetMCData(RegularGradAscData)
random_vs_mcsgradasc_cwc = playGames(100000,second_player = newPlayer(RegularGradAscData,MCSGradAsc),first_player = playerRandom,p_switch = TRUE)



plot(1:(100000/100),random_vs_mcsgradasc_cwc$winrate,col="blue",ylim = c(0,1),xlim = c(0,1000),
     main = "100000 games - Changing win conditions for every 30000th game",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")
points(1:(100000/100),random_vs_ipwga_cwc$winrate,col="red")
points(1:(100000/100),random_vs_mceps_cwc$winrate,col="green")
points(1:(100000/100),random_vs_eps_cwc$winrate,col="brown")
points(1:(100000/100),random_vs_cucb_cwc$winrate,col="gray")
points(1:(100000/100),random_vs_ceps_cwc$winrate,col="purple")
legend("bottom",
       c("Random policy starts vs. Monte Carlo gradient ascent policy",
         "Random policy starts vs. IPW gradient ascent policy",
         "Random policy starts vs. Monte Carlo epsilon-greedy policy",
         "Random policy starts vs. Epsilon-greedy policy",
         "Random policy starts vs. Contextual UCB policy", 
         "Random policy starts vs. Contextual epsilon-greedy"),
       fill=c("blue","red","green","brown","gray","purple"))


source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\game.R')
#source('C:\\Users\\hitma\\Dropbox\\Bachelorprojekt\\kode\\game.R')


ChangeWinningConditions <<- TRUE
global_alpha = 0.4
resetMCData(RegularGradAscData)
playerLearn.ResetData()
MCSGradAsc_vs_ipwga = playGames(100000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = newPlayer(NULL,IPWGradDesc),p_switch = FALSE)
playerLearn.ResetData()
resetMCData(RegularGradAscData)
ipwga_vs_MCSGradAsc = playGames(100000,second_player = newPlayer(RegularGradAscData,MCSGradAsc),first_player = newPlayer(NULL,IPWGradDesc),p_switch = FALSE)

plot(1:(100000/100),MCSGradAsc_vs_ipwga$winrate,col="blue",ylim = c(0,1),xlim = c(0,800),
     main = "80000 games - Monte Carlo gradient ascent policy vs. IPW gradient ascent policy",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")
points(1:(100000/100),ipwga_vs_MCSGradAsc$winrate,col="red")
legend("bottom",
       c("Win rate of the Monte Carlo gradient ascent policy where it plays first","Win rate of the IPW gradient ascent policy where it plays first"),
       fill=c("blue","red"))


# 300000 games

ChangeWinningConditions <<- TRUE
resetData(contextualUCBData)
cucb_vs_random_cwc = playGames(300000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = playerRandom,p_switch = FALSE)
ChangeWinningConditions <<- TRUE
resetData(contextualEpsData)
ceps_vs_random_cwc = playGames(300000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerRandom,p_switch = FALSE)
ChangeWinningConditions <<- TRUE
eps_vs_random_cwc = playGames(300000,first_player = newPlayer(epsilonData,epsGreedyAction),second_player = playerRandom,p_switch = FALSE)
ChangeWinningConditions <<- TRUE
resetMCData(MCEpsSoftData)
mceps_vs_random_cwc = playGames(300000,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),second_player = playerRandom,p_switch = FALSE)
ChangeWinningConditions <<- TRUE
playerLearn.ResetData()
ipwga_vs_random_cwc = playGames(300000,first_player = newPlayer(NULL,IPWGradDesc),second_player = playerRandom,p_switch = FALSE)
ChangeWinningConditions <<- TRUE
global_alpha <<- 0.4
resetMCData(RegularGradAscData)
mcsgradasc_vs_random_cwc = playGames(300000,first_player = newPlayer(RegularGradAscData,MCSGradAsc),second_player = playerRandom,p_switch = FALSE)



plot(1:(300000/100),mcsgradasc_vs_random_cwc$winrate,col="blue",ylim = c(0,1),xlim = c(0,3000),
     main = "300000 games - Changing win conditions for every 150000th game",
     xlab = "Number of games divided by 100",
     ylab = "Win rate")
points(1:(300000/100),ipwga_vs_random_cwc$winrate,col="red")
points(1:(300000/100),mceps_vs_random_cwc$winrate,col="green")
points(1:(300000/100),eps_vs_random_cwc$winrate,col="brown")
points(1:(300000/100),cucb_vs_random_cwc$winrate,col="gray")
points(1:(300000/100),ceps_vs_random_cwc$winrate,col="purple")
legend("bottom",
       c("Monte Carlo gradient ascent policy starts vs. Random policy",
         "IPW gradient ascent policy starts vs. Random policy",
         "Monte Carlo epsilon-greedy policy starts vs. Random policy",
         "Epsilon-greedy policy starts vs. Random policy",
         "Contextual UCB policy starts vs. Random policy", 
         "Contextual epsilon-greedy starts vs. Random policy"),
       fill=c("blue","red","green","brown","gray","purple"))