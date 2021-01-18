source('~/Dropbox/Bachelorprojekt/kode/game.R')


resetMCData(MCEpsSoftData)
resetData(contextualUCBData)
mceps_vs_cucb = playGames(20000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),p_switch = TRUE)
resetData(contextualUCBData)
resetMCData(MCEpsSoftData)
cucb_vs_mceps = playGames(20000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),p_switch = FALSE)
plot(1:(20000/100),mceps_vs_cucb$winrate,col="blue",ylim = c(0,1),xlim = c(0,200),
     main = "20000 games - Contextual UCB strategy starts vs. Monte Carlo and vice versa",
     xlab = "Number of games divided by 100",
     ylab = "winrate for 100 games in a row")
points(1:(20000/100),cucb_vs_mceps$winrate,col="red")
legend("topright",
       c("MC strategy winrate","Contextual UCB strategy winrate"),
       fill=c("blue","red"))

resetMCData(MCEpsSoftData)
resetData(contextualUCBData)
mceps_vs_cucb_r = playGames(20000,second_player = newPlayer(contextualUCBData,contextualUCBAction),first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),p_switch = FALSE)
resetData(contextualUCBData)
resetMCData(MCEpsSoftData)
cucb_vs_mceps_r = playGames(20000,second_player = newPlayer(contextualUCBData,contextualUCBAction),first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),p_switch = TRUE)
plot(1:(20000/100),mceps_vs_cucb_r$winrate,col="blue",ylim = c(0,1),xlim = c(0,200),
     main = "20000 games - Monte Carlo starts vs. Contextual UCB strategy and vice versa",
     xlab = "Number of games divided by 100",
     ylab = "winrate for 100 games in a row")
points(1:(20000/100),cucb_vs_mceps_r$winrate,col="red")
legend("topright",
       c("MC strategy winrate","Contextual UCB strategy winrate"),
       fill=c("blue","red"))

resetMCData(MCEpsSoftData)
resetData(contextualUCBData)

global_epsilon <<- 0.05
mceps_vs_random = playGames(20000,second_player = playerRandom,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),p_switch = FALSE)
global_epsilon <<- 0.0
mceps_vs_random2 = playGames(5000,second_player = playerRandom,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),p_switch = FALSE)

plot(1:(20000/100),mceps_vs_random$winrate,col="blue",ylim = c(0,1),xlim = c(0,400),
     main = "20000 games - Monte Carlo starts vs. Random",
     xlab = "Number of games divided by 100",
     ylab = "winrate for 100 games in a row")

plot(1:(5000/100),mceps_vs_random2$winrate,col="blue",ylim = c(0,1),xlim = c(0,200),
     main = "20000 games - Monte Carlo starts vs. Random",
     xlab = "Number of games divided by 100",
     ylab = "winrate for 100 games in a row")
#Play 2 monte carlo agiasnt each other
# 

global_epsilon <<- 0.05
left_vs_eps = playGames(20000,first_player = playerLeft,second_player = newPlayer(epsilonData,epsGreedyAction),p_switch = TRUE)
global_epsilon <<- 0.0
left_vs_eps2 = playGames(5000,first_player = playerLeft,second_player = newPlayer(epsilonData,epsGreedyAction),p_switch = TRUE)

plot(1:(20000/100),left_vs_eps$winrate,col="blue",ylim = c(0,1),xlim = c(0,00),
     main = "20000 games - Monte Carlo starts vs. Random",
     xlab = "Number of games divided by 100",
     ylab = "winrate for 100 games in a row")
plot(1:(5000/100),left_vs_eps2$winrate,col="blue",ylim = c(0,1),xlim = c(0,200),
     main = "20000 games - Monte Carlo starts vs. Random",
     xlab = "Number of games divided by 100",
     ylab = "winrate for 100 games in a row")


resetMCData(MCEpsSoftData)
resetData(contextualUCBData)
mceps_vs_left = playGames(20000,second_player = playerLeft,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),p_switch = FALSE)
global_epsilon <<- 0.0
mceps_vs_left2 = playGames(20000,second_player = playerLeft,first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),p_switch = FALSE)



plot(1:(20000/100),mceps_vs_left$winrate,col="blue",ylim = c(0,1),xlim = c(0,200),
     main = "20000 games - Monte Carlo starts vs. Left",
     xlab = "Number of games divided by 100",
     ylab = "winrate for 100 games in a row")

plot(1:(20000/100),mceps_vs_left2$winrate,col="blue",ylim = c(0,1),xlim = c(0,200),
     main = "20000 games - Monte Carlo starts vs. Left",
     xlab = "Number of games divided by 100",
     ylab = "winrate for 100 games in a row")

# Left player starts vs. non-contextual ucb
left_vs_UCB = playGames(20000,first_player = playerLeft,second_player = newPlayer(UCBData,UCBAction),p_switch = TRUE)
UCB_vs_left = playGames(20000,first_player = newPlayer(UCBData,UCBAction),second_player = playerLeft,p_switch = FALSE)


#Why converge to 0.5 when not starting?
plot(1:(20000/100),UCB_vs_left$winrate,col="blue",ylim = c(0,1),xlim = c(0,200),
     main = "20000 games - UCB strategy starts vs. Left strategy and vice versa",
     xlab = "Number of games divided by 100",
     ylab = "UCB winrate for 100 games in a row")
points(1:(20000/100),left_vs_UCB$winrate,col="red")
legend("bottomright",
       c("UCB strategy starts","Left strategy starts"),
       fill=c("blue","red")
       
)


##
# Run games
##

#Non-contextual UCB starts vs left
UCB_vs_eps = playGames(20000,first_player = newPlayer(UCBData,UCBAction),second_player = newPlayer(epsilonData,epsGreedyAction))
plot(1:(20000/100),UCB_vs_eps$winrate,col="blue")
max(UCB_vs_eps$wins)
max(UCB_vs_eps$losses)
max(UCB_vs_eps$draws)

eps_vs_UCB = playGames(20000,first_player = newPlayer(epsilonData,epsGreedyAction),second_player = newPlayer(UCBData,UCBAction))
plot(1:(20000/100),eps_vs_UCB$winrate,col="blue")
max(eps_vs_UCB$wins)
max(eps_vs_UCB$losses)
max(eps_vs_UCB$draws)

#Non-contextual epsilon starts vs left
global_epsilon <<- 0.05
left_vs_eps005 = playGames(40000,first_player = playerLeft,second_player = newPlayer(epsilonData,epsGreedyAction),p_switch = TRUE)
eps_vs_left005 = playGames(20000,first_player = newPlayer(epsilonData,epsGreedyAction),second_player = playerRandom)
global_epsilon <<- 0.15
left_vs_eps015 = playGames(20000,first_player = playerRandom,second_player = newPlayer(epsilonData,epsGreedyAction),p_switch = TRUE)
eps_vs_left015 = playGames(20000,first_player = newPlayer(epsilonData,epsGreedyAction),second_player = playerRandom)
global_epsilon <<- 0.10
left_vs_eps010 = playGames(20000,first_player = playerRandom,second_player = newPlayer(epsilonData,epsGreedyAction),p_switch = TRUE)
eps_vs_left010 = playGames(20000,first_player = newPlayer(epsilonData,epsGreedyAction),second_player = playerRandom)
global_epsilon <<- 0.01
left_vs_eps001 = playGames(20000,first_player = playerRandom,second_player = newPlayer(epsilonData,epsGreedyAction),p_switch = TRUE)
eps_vs_left001 = playGames(20000,first_player = newPlayer(epsilonData,epsGreedyAction),second_player = playerRandom)

plot(1:(20000/100),eps_vs_left005$winrate,col="blue",
     main = "20000 games - Epsilon-Greedy strategy starts vs. Random strategy", 
     xlab = "Number of games divided by 100", 
     ylab = "Epsilon-Greedy winrate for 100 games in a row",ylim = c(0,1),xlim = c(0,200))
points(1:(20000/100),eps_vs_left001$winrate,col="green")
points(1:(20000/100),eps_vs_left015$winrate,col="red")
points(1:(20000/100),eps_vs_left010$winrate,col="purple")
legend("bottomright",
       c("epsilon = 0.01","epsilon = 0.05","epsilon = 0.10", "epsilon = 0.15"),
       fill=c("green","blue","purple","red")
)

#Why converge to 0.5 when not starting?
  plot(1:(40000/100),left_vs_eps005$winrate,col="blue",
       main = "20000 games - Random strategy starts vs. Epsilon-Greedy strategy", 
       xlab = "Number of games divided by 100", 
       ylab = "Epsilon-Greedy winrate for 100 games in a row",ylim = c(0,1),xlim = c(0,400))
  points(1:(20000/100),left_vs_eps001$winrate,col="green")
  points(1:(20000/100),left_vs_eps015$winrate,col="red")
  points(1:(20000/100),left_vs_eps010$winrate,col="purple")
  legend("bottomright",
         c("epsilon = 0.01","epsilon = 0.05","epsilon = 0.10", "epsilon = 0.15"),
         fill=c("green","blue","purple","red")
  )

global_epsilon <<- 0.05
left_vs_ceps005 = playGames(20000,first_player = playerLeft,second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = TRUE)
resetData(contextualEpsData)
ceps_vs_left005 = playGames(20000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerLeft)
resetData(contextualEpsData)
global_epsilon <<- 0.15
left_vs_ceps015 = playGames(20000,first_player = playerLeft,second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = TRUE)
resetData(contextualEpsData)
ceps_vs_left015 = playGames(20000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerLeft)
resetData(contextualEpsData)
global_epsilon <<- 0.10
left_vs_ceps010 = playGames(20000,first_player = playerLeft,second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = TRUE)
resetData(contextualEpsData)
ceps_vs_left010 = playGames(20000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerLeft)
resetData(contextualEpsData)
global_epsilon <<- 0.01
left_vs_ceps001 = playGames(20000,first_player = playerLeft,second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = TRUE)
resetData(contextualEpsData)
ceps_vs_left001 = playGames(20000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerLeft)
resetData(contextualEpsData)

plot(1:(20000/100),ceps_vs_left005$winrate,col="blue",
     main = "20000 games - Contextual Epsilon-Greedy strategy starts vs. Left strategy", 
     xlab = "Number of games divided by 100", 
     ylab = "Epsilon-Greedy winrate for 100 games in a row",ylim = c(0,1),xlim = c(0,200))
points(1:(20000/100),ceps_vs_left001$winrate,col="green")
points(1:(20000/100),ceps_vs_left015$winrate,col="red")
points(1:(20000/100),ceps_vs_left010$winrate,col="purple")
legend("bottomright",
       c("epsilon = 0.01","epsilon = 0.05","epsilon = 0.10", "epsilon = 0.15"),
       fill=c("green","blue","purple","red")
)


plot(1:(20000/100),left_vs_ceps005$winrate,col="blue",
     main = "20000 games - Left strategy starts vs. Contextual Epsilon-Greedy strategy", 
     xlab = "Number of games divided by 100", 
     ylab = "Epsilon-Greedy winrate for 100 games in a row",ylim = c(0,1),xlim = c(0,200))
points(1:(20000/100),left_vs_ceps001$winrate,col="green")
points(1:(20000/100),left_vs_ceps015$winrate,col="red")
points(1:(20000/100),left_vs_ceps010$winrate,col="purple")
legend("bottomright",
       c("epsilon = 0.01","epsilon = 0.05","epsilon = 0.10", "epsilon = 0.15"),
       fill=c("green","blue","purple","red")
)


left_vs_cucb005 = playGames(20000,first_player = playerLeft,second_player = newPlayer(contextualUCBData,contextualUCBAction),p_switch = TRUE)
resetData(contextualUCBData)
cucb_vs_left005 = playGames(20000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = playerLeft)
resetData(contextualUCBData)

plot(1:(20000/100),cucb_vs_left005$winrate,col="blue",
     main = "20000 games - Contextual UCB strategy vs. Left strategy", 
     xlab = "Number of games divided by 100", 
     ylab = "Contextual UCB winrate for 100 games in a row",ylim = c(0,1),xlim = c(0,200))
points(1:(20000/100),left_vs_cucb005$winrate,col="green")
legend("bottomright",
       c("Contextual UCB starts","Left strategy starts"),
       fill=c("blue","green","purple","red")
)

global_epsilon <<- 0.05
random_vs_ceps005 = playGames(20000,first_player = playerRandom,second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = TRUE)
resetData(contextualEpsData)
ceps_vs_random005 = playGames(20000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerRandom)
resetData(contextualEpsData)
global_epsilon <<- 0.15
random_vs_ceps015 = playGames(20000,first_player = playerRandom,second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = TRUE)
resetData(contextualEpsData)
ceps_vs_random015 = playGames(20000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerRandom)
resetData(contextualEpsData)
global_epsilon <<- 0.10
random_vs_ceps010 = playGames(20000,first_player =playerRandom,second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = TRUE)
resetData(contextualEpsData)
ceps_vs_random010 = playGames(20000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerRandom)
resetData(contextualEpsData)
global_epsilon <<- 0.01
random_vs_ceps001 = playGames(20000,first_player = playerRandom,second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = TRUE)
resetData(contextualEpsData)
ceps_vs_random001 = playGames(20000,first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),second_player = playerRandom)
resetData(contextualEpsData)


plot(1:(20000/100),ceps_vs_random005$winrate,col="blue",
     main = "20000 games - Contextual Epsilon-Greedy strategy starts vs. Random strategy", 
     xlab = "Number of games divided by 100", 
     ylab = "Epsilon-Greedy winrate for 100 games in a row",ylim = c(0,1),xlim = c(0,200))
points(1:(20000/100),ceps_vs_random001$winrate,col="green")
points(1:(20000/100),ceps_vs_random015$winrate,col="red")
points(1:(20000/100),ceps_vs_random010$winrate,col="purple")
legend("bottomright",
       c("epsilon = 0.01","epsilon = 0.05","epsilon = 0.10", "epsilon = 0.15"),
       fill=c("green","blue","purple","red")
)


plot(1:(20000/100),random_vs_ceps005$winrate,col="blue",
     main = "20000 games - Random strategy starts vs. Contextual Epsilon-Greedy strategy", 
     xlab = "Number of games divided by 100", 
     ylab = "Epsilon-Greedy winrate for 100 games in a row",ylim = c(0,1),xlim = c(0,200))
points(1:(20000/100),random_vs_ceps001$winrate,col="green")
points(1:(20000/100),random_vs_ceps015$winrate,col="red")
points(1:(20000/100),random_vs_ceps010$winrate,col="purple")
legend("bottomright",
       c("epsilon = 0.01","epsilon = 0.05","epsilon = 0.10", "epsilon = 0.15"),
       fill=c("green","blue","purple","red")
)

random_vs_cucb = playGames(20000,first_player = playerRandom,second_player = newPlayer(contextualUCBData,contextualUCBAction),p_switch = TRUE)
resetData(contextualUCBData)
cucb_vs_random = playGames(20000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = playerLeft)
resetData(contextualUCBData)

plot(1:(20000/100),cucb_vs_random$winrate,col="blue",
     main = "20000 games - Contextual UCB strategy vs. Random strategy", 
     xlab = "Number of games divided by 100", 
     ylab = "Contextual UCB winrate for 100 games in a row",ylim = c(0,1),xlim = c(0,200))
points(1:(20000/100),random_vs_cucb$winrate,col="green")
legend("bottomright",
       c("Contextual UCB starts","Random strategy starts"),
       fill=c("blue","green","purple","red")
)

resetData(contextualUCBData)
resetData(contextualEpsData)
cucb_vs_ceps = playGames(20000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction))
resetData(contextualUCBData)
resetData(contextualEpsData)
cucb_vs_eps = playGames(20000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = newPlayer(epsilonData,epsGreedyAction))
resetData(contextualUCBData)
cucb_vs_ucb = playGames(20000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = newPlayer(UCBData,UCBAction))
resetData(contextualUCBData)
cucb_vs_left = playGames(20000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = playerLeft)
resetData(contextualUCBData)
cucb_vs_random = playGames(20000,first_player = newPlayer(contextualUCBData,contextualUCBAction),second_player = playerRandom)
resetData(contextualUCBData)

plot(1:(20000/100),cucb_vs_ceps$winrate,col="blue",
     main = "20000 games - Contextual UCB strategy starts vs. all other strategies", 
     xlab = "Number of games divided by 100", 
     ylab = "Contextual UCB winrate for 100 games in a row",ylim = c(0,1),xlim = c(0,200))
points(1:(20000/100),cucb_vs_eps$winrate,col="green")
points(1:(20000/100),cucb_vs_ucb$winrate,col="purple")
points(1:(20000/100),cucb_vs_left$winrate,col="red")
points(1:(20000/100),cucb_vs_random$winrate,col="gray")
legend("bottomright",
       c("Contexual UCB vs. Contextual Greedy-Epsilon","Contexual UCB vs. Epsilon-Greedy","Contexual UCB vs. UCB","Contexual UCB vs. Left","Contexual UCB vs. Random"),
       fill=c("blue","green","purple","red","gray")
)


resetData(contextualUCBData)
resetData(contextualEpsData)
ceps_vs_cucb = playGames(20000,second_player = newPlayer(contextualUCBData,contextualUCBAction),first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = TRUE)
resetData(contextualUCBData)
resetData(contextualEpsData)
eps_vs_cucb = playGames(20000,second_player = newPlayer(contextualUCBData,contextualUCBAction),first_player = newPlayer(epsilonData,epsGreedyAction),p_switch = TRUE)
resetData(contextualUCBData)
ucb_vs_cucb = playGames(20000,second_player = newPlayer(contextualUCBData,contextualUCBAction),first_player = newPlayer(UCBData,UCBAction),p_switch = TRUE)
resetData(contextualUCBData)
left_vs_cucb = playGames(20000,second_player = newPlayer(contextualUCBData,contextualUCBAction),first_player = playerLeft,p_switch = TRUE)
resetData(contextualUCBData)
random_vs_cucb = playGames(20000,second_player = newPlayer(contextualUCBData,contextualUCBAction),first_player = playerRandom,p_switch = TRUE)
resetData(contextualUCBData)
resetData(MCEpsSoftData)
mceps_vs_cucb = playGames(20000,second_player = newPlayer(contextualUCBData,contextualUCBAction),first_player = newPlayer(MCEpsSoftData,MCEpsSoftAction),p_switch = TRUE)
resetData(MCEpsSoftData)
resetData(contextualUCBData)

  plot(1:(20000/100),ceps_vs_cucb$winrate,col="blue",
       main = "20000 games - All other strategies start vs. Contextual UCB strategy", 
       xlab = "Number of games divided by 100", 
       ylab = "Contextual UCB winrate for 100 games in a row",ylim = c(0,1),xlim = c(0,200))
  points(1:(20000/100),eps_vs_cucb$winrate,col="green")
  points(1:(20000/100),ucb_vs_cucb$winrate,col="purple")
  points(1:(20000/100),left_vs_cucb$winrate,col="red")
  points(1:(20000/100),random_vs_cucb$winrate,col="gray")
  points(1:(20000/100),mceps_vs_cucb$winrate,col="orange")
  legend("top",
         c("Contexual UCB vs. Contextual Epsilon-Greedy","Contexual UCB vs. Epsilon-Greedy","Contexual UCB vs. UCB","Contexual UCB vs. Left","Contexual UCB vs. Random","Contexual UCB vs. Monte Carlo Epsilon-soft"),
         fill=c("blue","green","purple","red","gray","orange"),cex = 0.75
  )
max(ceps_vs_cucb$wins)


resetData(contextualEpsData)
ceps_vs_eps = playGames(20000,second_player = newPlayer(epsilonData,epsGreedyAction),first_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = FALSE)
resetData(contextualEpsData)
eps_vs_ceps = playGames(20000,first_player = newPlayer(epsilonData,epsGreedyAction),second_player = newPlayer(contextualEpsData,contextualEpsGreedyAction),p_switch = TRUE)
plot(1:(20000/100),ceps_vs_eps$winrate,col="blue",
     main = "20000 games - Contextual epsilon-greedy vs. Non-contextual epsilon-greedy", 
     xlab = "Number of games divided by 100", 
     ylab = "Contextual epsilon-greedy winrate for 100 games in a row",ylim = c(0,1),xlim = c(0,200))
points(1:(20000/100),eps_vs_ceps$winrate,col="red")
legend("bottom",c("Contextual epsilon-greedy starts","Contextual epsilon-greedy does not start"), fill=c("blue","red"))
