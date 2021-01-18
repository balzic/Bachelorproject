### Programming TicTacToe -- Causality Course 17/18
# Copyright (c) 2017-2018 Rune Christiansen [krunechristiansen@math.ku.dk]
#                         Jonas Peters  [jonas.peters@math.ku.dk]
# All rights reserved.  See the file COPYING for license terms. 

########
# The Game
########



game <- function(player1, player2, silent = FALSE){
  gameState <- rep(0,9)
  
  while(evaluateGameState(gameState)==42){
    movePlus1 <- do.call(player1, list(gameState))$move
    gameState[movePlus1] <- 1
    if(evaluateGameState(gameState)==42){
      moveMinus1 <- do.call(player2, list(-gameState))$move
      gameState[moveMinus1] <- -1
    }
  }
  evGS <- evaluateGameState(gameState)
  if(evGS==0){
    do.call(paste(player1, ".draw", sep = ""), list())
    do.call(paste(player2, ".draw", sep = ""), list())
    totalResults[totalGames] <<- 0 
    res <- 0 
    if(!silent){
      cat("Draw!")
      cat("\n\n")
      cat("Result:")
      cat("\n\n")
      print(matrix(gameState,3,3,byrow=T))
    }
  }
  if(evGS==1){
    res <- 1 
    do.call(paste(player1, ".win", sep = ""), list())
    do.call(paste(player2, ".loss", sep = ""), list())
    totalResults[totalGames] <<- 1 
    if(!silent){
      print("The winner is player 1")
      cat("\n\n")
      cat("Result:")
      cat("\n\n")
      print(matrix(gameState,3,3,byrow=T))
    }
  }
  if(evGS==-1){
    res <- -1 
    totalResults[totalGames] <<- -1 
    do.call(paste(player1, ".loss", sep = ""), list())
    do.call(paste(player2, ".win", sep = ""), list())
    if(!silent){
      print("The winner is player -1")
      cat("\n\n")
      cat("Result:")
      cat("\n\n")
      print(matrix(gameState,3,3,byrow=T))
    }
  }
  return(res)
}


evaluateGameState <- function(gameState, permut = (1:9)){
  gameState <- gameState[permut]
  gameStateMat <- matrix(gameState, 3, 3, byrow = T)
  rSums <- rowSums(gameStateMat)
  cSums <- colSums(gameStateMat)
  dSums <- c(sum(gameState[c(1,5,9)]), sum(gameState[c(3,5,7)]))
  sums  <- c(rSums, cSums, dSums)
  if(any(sums==3)) return(1)
  else if(any(sums==-3)) return(-1)
  else if(any(gameState==0)) return(42)
  else return(0)
}



########
# players
########

playerConsole <- function(gameState){ #playerConsole function that will make next move according to console input
  cat("The current game state is:")
  cat("\n\n")
  print(matrix(gameState,3,3,byrow=T))
  cat("\n\n")
  move <- readline(prompt = "Please make a move: ")
  return(list(move = as.integer(move)))
}

playerRandom <- function(gameState){
  move <- sample((1:9)[which(gameState==0)],1) #Choose 1 of the empty fields with equal probability.
  ngames <- historyRandom$n + 1 
  if(sum(gameState==0)>7){ 
    historyRandom$games[[ngames]] <<- c(gameState, move, 1/length(which(gameState==0)))
  }else{ 
    historyRandom$games[[ngames]] <<- rbind(historyRandom$games[[ngames]], c(gameState, move, 1/length(which(gameState==0)))) 
  }
  return(list(move = move)) #Return the drawn field.
}
playerRandom.draw <- function(){
  historyRandom$n <<- historyRandom$n + 1 #Update the number of games played
  historyRandom$results[historyRandom$n] <<- 0 #Insert 0 for draw in the results vector.
}
playerRandom.win <- function(){
  historyRandom$n <<- historyRandom$n + 1 
  historyRandom$results[historyRandom$n] <<- 1 #Insert 1 for playerRandom win in the results vector.
}
playerRandom.loss <- function(){
  historyRandom$n <<- historyRandom$n + 1 
  historyRandom$results[historyRandom$n] <<- -1 #Insert 0 for playerRandom loss in the results vector.
}

playerLeft <- function(gameState){ # playLeft function that will always choose the leftmost empty field in the current gameState.
  move <- which.min(abs(gameState)) # Choose the leftmost empty field in the gamestate.
  ngames <- historyLeft$n + 1 #let ngames be the current game number.
  if(sum(gameState==0)>7){ #If the number of moves made is stritly less than 2, meaning that one or both of the players have not made a move
    historyLeft$games[[ngames]] <<- c(gameState, move, 1) # Create a vector in historyLeft$games[[ngames]] corresponding the the current gamestate, what move is to be made, followed by at 1 (displayed the probability of the move made by playerLeft; nothing random).
  }else{ #If the number of moves made in the game is larger than or equal to 2, then 
    historyLeft$games[[ngames]] <<- rbind(historyLeft$games[[ngames]], c(gameState, move, 1)) # Create a new row in historyLeft$games[[ngames]] corresponding the the current gamestate, what move is to be made, followed by the probability for the move made.
  }
  return(list(move = move)) #return
}
playerLeft.draw <- function(){
  historyLeft$n <<- historyLeft$n + 1 #Update the number of games played
  historyLeft$results[historyLeft$n] <<- 0 #Insert 0 for draw in the results vector.
}
playerLeft.win <- function(){
  historyLeft$n <<- historyLeft$n + 1 
  historyLeft$results[historyLeft$n] <<- 1 #Insert 1 for playerLeft win in the results vector.
}
playerLeft.loss <- function(){
  historyLeft$n <<- historyLeft$n + 1 
  historyLeft$results[historyLeft$n] <<- -1 #Insert 0 for playerLeft loss in the results vector.
}





##########################
##########################
##########################
library(hash)


#########
# Exercise 2
#########
player1 <- "playerRandom"
player2 <- "playerLeft"  
numGames <- 10000
keepMin <- Inf
waitUntilStep <- Inf

strategyLearn <- hash()
historyLearn <- list(n = 0, results = NA, games = list())
historyLeft <- list(n = 0, results = NA, games = list())
historyRandom <- list(n = 0, results = NA, games = list())
totalGames <- 0
totalResults <- c()

set.seed(1)
for(i in 1:numGames){
  if(i %% 100 == 0) {
    show(i)
    plot((1:totalGames) - cumsum(I(totalResults==1)), xlim = c(0,numGames), ylim = c(0,numGames), ylab = "games not won", type = "l")
    Sys.sleep(0.02)
  }
  totalGames <- totalGames + 1
  game(player1 = player1, player2 = player2, silent = TRUE)
  
}
sum(historyRandom$results)


weights <- rep(1,historyRandom$n)
for(i in 1:historyRandom$n){
  gamee <- historyRandom$games[[i]]
  for(j in 1:(dim(gamee)[1])){
    if(gamee[j,10] == which.min(abs(gamee[j,1:9]))){
      weights[i] <- weights[i]/gamee[j,11]
    } else {
      weights[i] <- 0 
    }
  }
}
#point estimate
mean.est <- mean(weights*historyRandom$results)
#confidence interval
sd.est <- sd(weights*historyRandom$results)
c(mean.est - 1.96*sd.est/sqrt(historyRandom$n), mean.est, mean.est + 1.96*sd.est/sqrt(historyRandom$n))



#################################
## Implemention of playerLearn ##
#################################

playerLearn <- function(gameState){ # playerLearn function that act accordingly to what has been learned by previous games corresponding to the gradient strategy update.
  if(is.null(strategyLearn[[toString(gameState)]])){  # if the gameState has not been seen before, initialize with uniform distr.
    probmass <- rep(0,9) # Initialize zero mass on all fields.
    probmass[gameState == 0] <- 1/sum(gameState == 0) #Every empty field gets uniform prob. mass.
    strategyLearn[[toString(gameState)]] <<- log(probmass) #We save resulting log(probmass) into the strategyLearn corresponding to the previously unknown gameState.
  } else { #If the gameState has already seen 
    probmass <- exp(strategyLearn[[toString(gameState)]])/sum(exp(strategyLearn[[toString(gameState)]])) #then probmass becomes a vector of probabilities according to what has previously been learned.
  }
  #show(gameState)
  #show(strategyLearn[[toString(gameState)]])
  move <- sample(1:9, size=1, prob = probmass) #draw move with probabilities previously initialized.
  ngames <- historyLearn$n + 1 #let ngames be the current game number.
  if(sum(gameState==0)>7){  #If the number of moves made is stritly less than 2, meaning that one or both of the players have not made a move
    historyLearn$games[[ngames]] <<- c(gameState, move, probmass[move]) #Create a vector in historyLeft$games[[ngames]] corresponding the the current gamestate, what move is to be made, followed by the probability we made this move.
  }else{ #Otherwise add a row to the historyLearn$games[[ngames]] with the same information.
    historyLearn$games[[ngames]] <<- rbind(historyLearn$games[[ngames]], c(gameState, move, probmass[move]))
  }
  return(list(move = move))
}

test = matrix(data = c(rep(0,9),1,0.1))
dim(test)[1]
historyLearn$games[[2]]
test = rbind(c(rep(0,9),2,0.50),c(rep(0,9),2,0.50))
test = rbind(test,c(rep(0,9),2,0.50))
test = rbind(test,c(rep(0,9),2,0.50))
test

playerLearn.update <- function(){
  stepsize <- 20/historyLearn$n
  
  ####
  # compute gradient
  ####  
  if((totalGames >  waitUntilStep) && ((totalGames %% doStepEvery) == doStepEvery-1)){ #update strategy
    gradientLearn <- copy(strategyLearn)
    .set(gradientLearn, keys(gradientLearn), rep(list(rep(0,9)),length(keys(gradientLearn))))
    for(i in 1:historyLearn$n){
      
      gamee <- historyLearn$games[[i]]
      wup <- 1
      wdown <- 1
      
      for(j in 1:dim(gamee)[1]){
        #gamestate
        gs <- gamee[j,1:9]
        # action taken
        ac = gamee[j,10]
        # wup (W nominator) looks at current probabilities. Going through the for loop this will be the product of all probabilities for actions taken under the current strategy. 
        wup <- wup * exp(strategyLearn[[toString(gs)]][ac])/sum(exp(strategyLearn[[toString(gs)]]))
        # wdown (W denominator)looks at probabilities, under which the action was decided.  Going through the for loop this will be the product of all probabilities for actions taken under the data generation. 
        wdown <- wdown * gamee[j,11]
      }
      
      if(wup > 0){ #if games have zero prob. they are disregarded in the sum of the gradient.
        for(j in 1:dim(gamee)[1]){
          
          #j <- dim(gamee)[1]  WHY DONT WE ONLY USE THE LAST GAMESTATE IN EACH TERM AS THE FORMULA REQUIRES THE PLAYER TO GO THROUGH AT LEAST 4 STATES?????
          
          # get hashed game state
          gs <- gamee[j,1:9]
          ac <- gamee[j,10]
          
          
          gradientLearn[[toString(gs)]][ac] = gradientLearn[[toString(gs)]][ac] + 
            historyLearn$results[i] * 1/wup * (-1 + exp(strategyLearn[[toString(gs)]][ac])/sum(exp(strategyLearn[[toString(gs)]])))
          
          others <- setdiff(which(strategyLearn[[toString(gs)]] > -Inf), ac)
          
          gradientLearn[[toString(gs)]][others] = gradientLearn[[toString(gs)]][others] - 
            historyLearn$results[i] * 1/(1-wup) * exp(strategyLearn[[toString(gs)]][others])/sum(exp(strategyLearn[[toString(gs)]]))
        }
      }
    }
    ####
    ## makegradient step
    ####  
    for(i in keys(strategyLearn)){
      newVector <- strategyLearn[[i]] + stepsize * gradientLearn[[i]]
      newVector <- newVector - mean(newVector[newVector > -Inf])
      if(max(newVector) > 20){
        newVector <- newVector * 20 / max(newVector)
      }
      .set(strategyLearn, i, newVector)
    }
  }
  
  ####
  ## clear history to keep only the last "keepMin" games
  ####  
  if((totalGames > keepMin) && ((totalGames %% clearEvery) == (clearEvery - 1))){
    #clear history
    historyLearn$games <<- historyLearn$games[(historyLearn$n - keepMin + 1):historyLearn$n]
    historyLearn$results <<- historyLearn$results[(historyLearn$n - keepMin + 1):historyLearn$n]
    
    historyLearn$n <<- keepMin
  }
}

playerLearn.draw <- function(){
  historyLearn$n <<- historyLearn$n + 1
  historyLearn$results[historyLearn$n] <<- 0
  playerLearn.update()
}
playerLearn.win <- function(){
  historyLearn$n <<- historyLearn$n + 1
  historyLearn$results[historyLearn$n] <<- 1
  playerLearn.update()
}
playerLearn.loss <- function(){
  historyLearn$n <<- historyLearn$n + 1
  historyLearn$results[historyLearn$n] <<- -1
  playerLearn.update()
}



#########
# Exercise 3
#########
library(hash)
numGames <- 5000
clearEvery <- 500
keepMin <- 500
waitUntilStep <- 500
doStepEvery <- 500

player1 <- "playerLearn" 
player2 <- "playerLeft"

strategyLearn <- hash()
historyLearn <- list(n = 0, results = NA, games = list())
historyLeft <- list(n = 0, results = NA, games = list())
historyRandom <- list(n = 0, results = NA, games = list())
totalGames <- 0
totalResults <- c()

str(historyLearn)

set.seed(1)
for(i in 1:numGames){
  if(i %% 100 == 0) {
    show(i)
    plot((1:totalGames) - cumsum(I(totalResults==1)), xlim = c(0,numGames), ylim = c(0,numGames), ylab = "games not won", type = "l")
    Sys.sleep(0.02)
  }
  
  totalGames <- totalGames + 1
  res <- game(player1 = player1, player2 = player2, silent = TRUE)
  if(totalGames==1){historyplayer1 <- res}
  else{historyplayer1 <- c(historyplayer1,res)}
}

sum(historyplayer1[4000:5000])


if(exercise == 3){
  show(strategyLearn[[toString(c(0,0,0,0,0,0,0,0,0))]])
  #[1]  6.344574  1.172104  0.906976 -1.315845 -1.644285 -1.406396 -1.385943 -1.515569 -1.155617
  show(strategyLearn[[toString(c(1,-1,0,0,0,0,0,0,0))]])
  #[1]        -Inf        -Inf -2.07063395 -1.62685525  1.78951089  0.03581545  1.18410159 -0.10717238  0.79523365
  show(strategyLearn[[toString(c(1,-1,-1,0,1,0,0,0,0))]])
  #[1]       -Inf       -Inf       -Inf  1.5222008       -Inf -1.0471729  0.1091111 -0.3235097 -0.2606293
  show(strategyLearn[[toString(c(1,-1,-1,1,1,-1,0,0,0))]])
  #[1]        -Inf        -Inf        -Inf        -Inf        -Inf        -Inf  0.03129616 -0.56379858  0.53250242
  show(strategyLearn[[toString(c(1,-1,-1,-1,1,1,-1,0,1))]])
  #NULL
}


#########
# Exercise 4
#########



numGames <- 80000
clearEvery <- 500
keepMin <- 500
waitUntilStep <- 500
doStepEvery <- 500

player1 <- "playerLearn" 
player2 <- "playerRandom"

strategyLearn <- hash()
historyLearn <- list(n = 0, results = NA, games = list())
historyLeft <- list(n = 0, results = NA, games = list())
historyRandom <- list(n = 0, results = NA, games = list())
totalGames <- 0
totalResults <- c()

str(historyLearn)

set.seed(1)
for(i in 1:numGames){
  if(i %% 100 == 0) {
    show(i)
    plot((1:totalGames) - cumsum(totalResults), xlim = c(0,numGames), ylim = c(0,numGames), ylab = "games not won", type = "l")
    Sys.sleep(0.02)
  }
  
  totalGames <- totalGames + 1
  res <- game(player1 = player1, player2 = player2, silent = TRUE)
  if(totalGames==1){historyplayer1 <- res}
  else{historyplayer1 <- c(historyplayer1,res)}
}

sum(historyplayer1[75000:80000])


if(exercise == 4){
  strategyLearn[[toString(c(0,0,0,0,0,0,0,0,0))]]
  #[1] -0.9779571 -1.2351388 -0.6705408 -1.2249175  8.2869045 -1.5449792 -0.2296560 -1.7594817 -0.6442335
  strategyLearn[[toString(c(0,-1,0,0,1,0,0,0,0))]]
  #[1]  5.0680489       -Inf  0.3549637 -0.4640398       -Inf -0.9847218 -1.0086914 -1.9407190 -1.0248406
  strategyLearn[[toString(c(1,-1,0,0,1,0,0,0,-1))]]
  #[1]        -Inf        -Inf -0.69377354  0.02406757        -Inf -0.31169288  2.61842935 -1.63703050        -Inf
  strategyLearn[[toString(c(1,-1,-1,0,1,0,1,0,-1))]]
  #[1]      -Inf      -Inf      -Inf  2.014145      -Inf -0.565782      -Inf -1.448363      -Inf
  
  strategyLearn[[toString(c(0,0,0,0,0,0,0,0,0))]]
  #[1] -0.9779571 -1.2351388 -0.6705408 -1.2249175  8.2869045 -1.5449792 -0.2296560 -1.7594817 -0.6442335
  strategyLearn[[toString(c(-1,0,0,0,1,0,0,0,0))]]
  #[1]       -Inf  0.4971553 -1.0853846 -0.6429925       -Inf -0.7392952  5.2713364 -1.2692715 -2.0315478
  strategyLearn[[toString(c(-1,0,-1,0,1,0,1,0,0))]]
  #[1]       -Inf  3.5895390       -Inf -1.1815160       -Inf -0.7113379       -Inf -0.6103888 -1.0862963
  strategyLearn[[toString(c(-1,1,-1,0,1,0,1,-1,0))]]
  #[1]       -Inf       -Inf       -Inf       -Inf       -Inf  1.1295500 -0.7379542 -0.3915958       -Inf
}






