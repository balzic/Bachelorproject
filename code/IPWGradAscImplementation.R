### ALL THE CODE IN THIS FILE IS NOT MY ORIGINAL WORK.
### THIS CODE HAS BEEN HANDED OUT TO ME BY MY SUPERVISOR JONAS PETERS

library(hash)
### IPW GRAD DATA
clearEvery <- 500
keepMin <- 500
waitUntilStep <- 500
doStepEvery <- 500

strategyLearn <- hash()
historyLearn <- list(n = 0, results = NA, games = list())
historyLeft <- list(n = 0, results = NA, games = list())
historyRandom <- list(n = 0, results = NA, games = list())
totalGames <- 0
### IPW GRAD DATA

playerLearn.ResetData <- function() {
  clear(strategyLearn)
  historyLearn <<- list(n = 0, results = NA, games = list())
  historyLeft <<- list(n = 0, results = NA, games = list())
  historyRandom <<- list(n = 0, results = NA, games = list())
  totalGames <<- 0
}

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
            historyLearn$results[i] * wup/wdown * (1 - exp(strategyLearn[[toString(gs)]][ac])/sum(exp(strategyLearn[[toString(gs)]])))
          
          others <- setdiff(which(strategyLearn[[toString(gs)]] > -Inf), ac)
          
          gradientLearn[[toString(gs)]][others] = gradientLearn[[toString(gs)]][others] - 
            historyLearn$results[i] * wup/wdown * exp(strategyLearn[[toString(gs)]][others])/sum(exp(strategyLearn[[toString(gs)]]))
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