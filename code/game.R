source('C:\\Users\\hitma\\Dropbox\\Bachelorprojekt\\kode\\IPWGradAscImplementation.R')
#source('C:\\Users\\Christian\\Dropbox\\Bachelorprojekt\\kode\\IPWGradAscImplementation.R')

library(hash)
########
# The Game
########


global_ngames <<- 0
ChangeWinningConditions <<- FALSE
ChangeWinningConditionsInterval <<- 60000
ChangedConditions <<- FALSE
radical <<- FALSE

## 0 - undecided, 1 - strat1, 2 - strat2
checkVictory = function(gameboard_in){
  gameboard = matrix(gameboard_in,3,3)
  rSums <- rowSums(gameboard)
  cSums <- colSums(gameboard)
  dSums <- c(gameboard[1,1] + gameboard[2,2] + gameboard[3,3]
          ,gameboard[1,3] + gameboard[2,2] + gameboard[3,1])
  sums  <- c(rSums, cSums, dSums) # Normal wincond
  if (ChangeWinningConditions && ((global_ngames + 1)%% ChangeWinningConditionsInterval) == 0)
  {
    if (ChangedConditions)
    {
      ChangedConditions <<- FALSE
    }
    else 
    {
      ChangedConditions <<- TRUE
    }
  }
  if (ChangedConditions)
  {
    if (radical) 
    {
      ## radical
      sums <- c(gameboard[1,1]+gameboard[3,2]+gameboard[2,3],
                gameboard[3,1]+gameboard[1,2]+gameboard[2,3],
                gameboard[1,2]+gameboard[2,1]+gameboard[3,3],
                gameboard[1,3]+gameboard[2,1]+gameboard[3,2])
    }
    else 
    {
      ## different
      sums  <- c(gameboard[1,1]+gameboard[3,2]+gameboard[1,3],
                 gameboard[3,1]+gameboard[1,2]+gameboard[3,3],
                 gameboard[1,1]+gameboard[2,3]+gameboard[3,1],
                 gameboard[1,3]+gameboard[2,1]+gameboard[3,3])
    }
  }
  if(any(sums==3)) return(1)
  else if(any(sums==-3)) return(-1)
  else return(0)
}

extractActionSet = function(gameboard){
  which(gameboard %in% c(0))
}
# player1 starts and we are by default playing as player1.
# We can switch to playing as player2 by calling game with switch = true
game = function(player1, player2, switch = FALSE){
    
    strat1point = 1 
    strat2point = -1
    player1action = NULL
    player2action = NULL
    playerWon = 0
    gameboard = rep(0,9)
    plays = 0
    # Play until a winner is reached or the total plays reach 8
    while (playerWon == 0 && plays < 10)
    {
      player1action = player1$action(gameboard,player1action,strat1point,player1)
      gameboard[[player1action]] = strat1point
      playerWon = checkVictory(gameboard)
      if (playerWon == strat1point)
      {
        gameboard[[player1action]] = 0
        player2$action(gameboard, player2action, strat2point,player2,lost=TRUE)
        break
      }
      plays = plays + 1
      
      if (plays >= 9)
      {
        gameboard[[player2action]] = 0
        player1$action(gameboard,player1action,strat1point,player1,lost=FALSE,draw= TRUE)
        gameboard[[player2action]] = strat2point
        gameboard[[player1action]] = 0
        player2$action(gameboard, player2action, strat2point,player2,lost=FALSE,draw= TRUE)
        break
      }
      
      player2action = player2$action(gameboard, player2action, strat2point,player2)
      gameboard[[player2action]] = strat2point
      playerWon = checkVictory(gameboard)
      if (playerWon == strat2point)
      {
        gameboard[[player2action]] = 0
        player1$action(gameboard,player1action,strat1point,player1,lost=TRUE)
        break
      }
      plays = plays + 1
      
    }
    
    if (!switch && playerWon == 1)
    {
      c(0,1,0)
    }
    else if (switch && playerWon == -1)
    {
      c(0,1,0)
    }
    else if (plays >= 9)
    {
      c(1,0,0)
    }
    else
    {
      c(0,0,1)
    }
    
}
##
# DEFINE STRATEGIES 
##
epsGreedyActionImproved = function(in_gameboard, prev_action, point, player, lost = FALSE, draw = FALSE){
  real_gameboard = in_gameboard
  gameboard = replace(in_gameboard, in_gameboard==point*(-1), 0)
  if (lost) # You lost
  {
    old_gameboard = gameboard
    old_gameboard[[prev_action]] = 0
    old_performance = player$data[[toString(old_gameboard)]][[prev_action]]
    player$data[[toString(old_gameboard)]][[prev_action]] = old_performance - 1
    player$data[[toString(gameboard)]] = rep(0,9)
    0
  }
  else # Make a move
  {
    ## Make sure the current gameboard exists in memory
    if (is.null(player$data[[toString(gameboard)]]))
    {
      player$data[[toString(gameboard)]] = rep(0,9)
    }
    ## Update return on available actions
    actionSet = extractActionSet(real_gameboard)
    for (a in actionSet)
    {
        temp_gameboard = gameboard
        temp_gameboard[[a]] = point
        if (is.null(player$data[[toString(temp_gameboard)]]))
        {
          player$data[[toString(temp_gameboard)]] = rep(0,9)
        }
        theReturn = sum(player$data[[toString(temp_gameboard)]])
        if (!(all(0==temp_gameboard)))
        {
          player$data[[toString(gameboard)]][[a]] = theReturn
        }
    }
    ## Take the most profitable action or a random one
    action = 0
    p = runif(1)
    epsilon = 0.15
    if (p < epsilon)
    {
      #actionSet must be larger than 1
      if (length(actionSet) > 1) 
      {
        action = sample(actionSet, 1)
      }
      else 
      {
        action = actionSet[1]
      }
    }
    else 
    {
      temp_table = player$data[[toString(gameboard)]]
      #actionSet must be larger than 1
      while (!(action %in% actionSet))
        #choose most rewarding action
      {
        if (action != 0)
        {
          temp_table[[action]] = -10000
        }
        action = which.max(temp_table)
      }
    }
    ## Check if you won with that action, if you win update the return
    new_gameboard = real_gameboard
    new_gameboard[[action]] = point
    if (checkVictory(new_gameboard))
    {
      player$data[[toString(gameboard)]][[action]] = player$data[[toString(gameboard)]][[action]] + 1
    }
    action
  }
}
global_epsilon <<- 0.05
epsGreedyAction = function(gameboard, prev_action, point, player, lost = FALSE, draw = FALSE){
  if (lost) # You lost
  {
    player$data$return[[prev_action]] = player$data$return[[prev_action]] + 
        (1/5) * (-1 - player$data$return[[prev_action]])
    
  }
  else if (draw) 
  {
    # Do nothing
  }
  else # Make a move
  {
    actionSet = extractActionSet(gameboard)
    ## Take the most profitable action or a random one
    action = 0
    p = runif(1)
    epsilon = global_epsilon
    ## Determine ACTION
    if (p < epsilon)
    {
      #actionSet must be larger than 1
      if (length(actionSet) > 1) 
      {
        action = sample(actionSet, 1)
      }
      else 
      {
        action = actionSet[1]
      }
    }
    else 
    {
      temp_table = player$data$return
      while (!(action %in% actionSet))
        #choose most rewarding action
      {
        if (action != 0)
        {
          temp_table[[action]] = -1000000L
        }
        action = which.max(rank(temp_table, ties.method = "random"))
      }
    }
    ##PLAY THE ACTION
    new_gameboard = gameboard
    new_gameboard[[action]] = point
    player$data$n_actions[[action]] = player$data$n_actions[[action]] + 1
    ## Check if you won with that action
    if (checkVictory(new_gameboard) == point)
    {
      ## The less weight we give to a win, in comparison to a loss, the more we win and the less we lose.
      # In this sense, we play more safely when we increase the difference between the weights,
      # however we also get more draws
      player$data$return[[action]] = player$data$return[[action]] + 
        (1/5) * (1 - player$data$return[[action]])
    }
    else
    {
      player$data$return[[action]] = player$data$return[[action]] + 
        (1/5) * (0 - player$data$return[[action]])
    }
    action
  }
}

contextualUCBAction = function(gameboard, prev_action, point, player, lost = FALSE, draw = FALSE){
  gm = toString(gameboard)
  if (lost) # You lost
  {
    gameboard[[prev_action]] = 0
    gm = toString(gameboard)
    player$data$return[[gm]][[prev_action]] = player$data$return[[gm]][[prev_action]] + 
      (1/5) * (-1 - player$data$return[[gm]][[prev_action]])
    
  }
  else if (draw) # You lost
  {
    # Do nothing
  }
  else # Make a move
  {
    if (is.null(player$data$n_actions[[gm]]))
    {
      player$data$n_actions[[gm]] = rep(0,9)
    }
    if (is.null(player$data$return[[gm]]))
    {
      player$data$return[[gm]] = rep(0,9)
    }
    actionSet = extractActionSet(gameboard)
    ## Take the most profitable action or a random one
    action = 0
    ## Determine ACTION
    temp_table = player$data$return[[gm]]
    c = 0.10
    maximizing = which(player$data$n_actions[[gm]] == 0)
    temp_return = player$data$return[[gm]]
    temp_return[maximizing] = 1000000L
    temp_n_actions = player$data$n_actions[[gm]]
    temp_n_actions[maximizing] = 1
    t = sum(temp_n_actions)
    options = temp_return + c*sqrt(log(t)/temp_n_actions)
    while (!(action %in% actionSet))
      #choose most rewarding action
    {
      if (action != 0)
      {
        options[[action]] = -1000000L
      }
      action = which.max(rank(options, ties.method = "random"))
    }
    ##PLAY THE ACTION
    new_gameboard = gameboard
    new_gameboard[[action]] = point
    player$data$n_actions[[gm]][[action]] = player$data$n_actions[[gm]][[action]] + 1
    ## Check if you won with that action
    if (checkVictory(new_gameboard) == point)
    {
      ## The less weight we give to a win, in comparison to a loss, the more we win and the less we lose.
      # In this sense, we play more safely when we increase the difference between the weights,
      # however we also get more draws
      player$data$return[[gm]][[action]] = player$data$return[[gm]][[action]] + 
        (1/5) * (1 - player$data$return[[gm]][[action]])
    }
    else 
    {
      player$data$return[[gm]][[action]] = player$data$return[[gm]][[action]] + 
        (1/5) * (0 - player$data$return[[gm]][[action]])
    }
    action
  }
}

contextualEpsGreedyAction = function(gameboard, prev_action, point, player, lost = FALSE, draw = FALSE){
  gm = toString(gameboard)
  if (lost) # You lost
  {
    gameboard[[prev_action]] = 0
    gm = toString(gameboard)
    player$data$return[[gm]][[prev_action]] = player$data$return[[gm]][[prev_action]] + 
      (1/5) * (-1 - player$data$return[[gm]][[prev_action]])
    
  }
  else if (draw) 
  {
    # Do nothing
  }
  else # Make a move
  {
    if (is.null(player$data$n_actions[[gm]]))
    {
      player$data$n_actions[[gm]] = rep(0,9)
    }
    if (is.null(player$data$return[[gm]]))
    {
      player$data$return[[gm]] = rep(0,9)
    }
    actionSet = extractActionSet(gameboard)
    ## Take the most profitable action or a random one
    action = 0
    p = runif(1)
    epsilon = global_epsilon
    ## Determine ACTION
    if (p < epsilon)
    {
      #actionSet must be larger than 1
      if (length(actionSet) > 1) 
      {
        action = sample(actionSet, 1)
      }
      else 
      {
        action = actionSet[1]
      }
    }
    else 
    {
      temp_table = player$data$return[[gm]]
      while (!(action %in% actionSet))
        #choose most rewarding action
      {
        if (action != 0)
        {
          temp_table[[action]] = -1000000L
        }
        action = which.max(rank(temp_table, ties.method = "random"))
      }
    }
    ##PLAY THE ACTION
    new_gameboard = gameboard
    new_gameboard[[action]] = point
    player$data$n_actions[[gm]][[action]] = player$data$n_actions[[gm]][[action]] + 1
    ## Check if you won with that action
    if (checkVictory(new_gameboard) == point)
    {
      ## The less weight we give to a win, in comparison to a loss, the more we win and the less we lose.
      # In this sense, we play more safely when we increase the difference between the weights,
      # however we also get more draws
      player$data$return[[gm]][[action]] = player$data$return[[gm]][[action]] + 
        (1/5) * (1 - player$data$return[[gm]][[action]])
    }
    else 
    {
      player$data$return[[gm]][[action]] = player$data$return[[gm]][[action]] + 
        (1/5) * (0 - player$data$return[[gm]][[action]])
    }
    action
  }
}


IPWGradDesc = function(gameState, prev_action, point, player, lost = FALSE, draw = FALSE){
  totalGames <<- totalGames + 1
  if (lost) # You lost
  {
    historyLearn$n <<- historyLearn$n + 1
    historyLearn$results[historyLearn$n] <<- -1
    playerLearn.update()
  }
  else if (draw) # Draw
  {
    historyLearn$n <<- historyLearn$n + 1
    historyLearn$results[historyLearn$n] <<- 0
    playerLearn.update()
   
  }
  else # Make a move
  {
    
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
    ##PLAY THE ACTION
    new_gameboard = gameState
    new_gameboard[[move]] = point
    
    ## Check if you won with that action
    if (checkVictory(new_gameboard) == point)
    {
      historyLearn$n <<- historyLearn$n + 1
      historyLearn$results[historyLearn$n] <<- 1
      playerLearn.update()
    }
    
    move
  }
}

global_alpha <<- 0.1

MCSGradAsc = function(gameboard, prev_action, point, player, lost = FALSE, draw = FALSE){
  gm = toString(gameboard)
  if (lost) # You lost
  {
    for (gmboard in names(player$data$episode)) 
    {
      p_action = player$data$episode[[gmboard]]
      v_gmboard = as.integer(unlist(strsplit(gmboard,",")))
      others = extractActionSet(v_gmboard)
      others = others[others != p_action]
      reward = -1
      
      player$data$return[[gmboard]][[p_action]] = player$data$return[[gmboard]][[p_action]] + 
        (1/player$data$n_actions[[gmboard]][[p_action]]) * (reward - player$data$return[[gmboard]][[p_action]])
      
      avg_reward = player$data$return[[gmboard]]
      
      denominator_Policy = sum(exp(player$data$policy[[gmboard]]))
      
      p_action_Policy = exp(player$data$policy[[gmboard]][[p_action]])/denominator_Policy
      
      p_action_Grad = player$data$policy[[gmboard]][[p_action]] + global_alpha * (reward - avg_reward[[p_action]]) * (1-p_action_Policy)
      
      others_Policy = exp(player$data$policy[[gmboard]][others])/denominator_Policy
      
      others_Grad = player$data$policy[[gmboard]][others] - global_alpha * (reward - avg_reward[others]) * others_Policy
      
      player$data$policy[[gmboard]][[p_action]] = p_action_Grad
      
      player$data$policy[[gmboard]][others] = others_Grad
      
    }
    
    player$data$episode = list()
  }
  else if (draw) # You lost
  {
    for (gmboard in names(player$data$episode)) 
    {
      p_action = player$data$episode[[gmboard]]
      v_gmboard = as.integer(unlist(strsplit(gmboard,",")))
      others = extractActionSet(v_gmboard)
      others = others[others != p_action]
      reward = 0
      
      player$data$return[[gmboard]][[p_action]] = player$data$return[[gmboard]][[p_action]] + 
        (1/player$data$n_actions[[gmboard]][[p_action]]) * (reward - player$data$return[[gmboard]][[p_action]])
      
      avg_reward = player$data$return[[gmboard]]
      
      denominator_Policy = sum(exp(player$data$policy[[gmboard]]))
      
      p_action_Policy = exp(player$data$policy[[gmboard]][[p_action]])/denominator_Policy
      
      p_action_Grad = player$data$policy[[gmboard]][[p_action]] + global_alpha * (reward - avg_reward[[p_action]]) * (1-p_action_Policy)
      
      others_Policy = exp(player$data$policy[[gmboard]][others])/denominator_Policy
      
      others_Grad = player$data$policy[[gmboard]][others] - global_alpha * (reward - avg_reward[others]) * others_Policy
      
      player$data$policy[[gmboard]][[p_action]] = p_action_Grad
      
      player$data$policy[[gmboard]][others] = others_Grad
      
    }
    
    player$data$episode = list()
  }
  else # Make a move
  {
    actionSet = extractActionSet(gameboard)
    if (is.null(player$data$n_actions[[gm]]))
    {
      player$data$n_actions[[gm]] = rep(0,9)
    }
    if (is.null(player$data$return[[gm]]))
    {
      player$data$return[[gm]] = rep(0,9)
    }
    if (is.null(player$data$policy[[gm]]))
    {
      player$data$policy[[gm]] = rep(0,9)
      player$data$policy[[gm]][actionSet] = rep(1/length(actionSet),length(actionSet))
    }
    exp_policy = exp(player$data$policy[[gm]])/sum(exp(player$data$policy[[gm]])) 
    ## Take the most profitable action or a random one
    action = sample(1:9, 1, prob = exp_policy)
    ##PLAY THE ACTION
    new_gameboard = gameboard
    new_gameboard[[action]] = point
    player$data$n_actions[[gm]][[action]] = player$data$n_actions[[gm]][[action]] + 1
    ## Check if you won with that action
    player$data$episode[[gm]] = action
    if (checkVictory(new_gameboard) == point)
    {
      ## The less weight we give to a win, in comparison to a loss, the more we win and the less we lose.
      # In this sense, we play more safely when we increase the difference between the weights,
      # however we also get more draws
      for (gmboard in names(player$data$episode)) 
      {
        p_action = player$data$episode[[gmboard]]
        v_gmboard = as.integer(unlist(strsplit(gmboard,",")))
        others = extractActionSet(v_gmboard)
        others = others[others != p_action]
        reward = 1

        player$data$return[[gmboard]][[p_action]] = player$data$return[[gmboard]][[p_action]] + 
          (1/player$data$n_actions[[gmboard]][[p_action]]) * (reward - player$data$return[[gmboard]][[p_action]])
        
        avg_reward = player$data$return[[gmboard]]
        
        denominator_Policy = sum(exp(player$data$policy[[gmboard]]))
        
        p_action_Policy = exp(player$data$policy[[gmboard]][[p_action]])/denominator_Policy
        
        p_action_Grad = player$data$policy[[gmboard]][[p_action]] + global_alpha * (reward - avg_reward[[p_action]]) * (1-p_action_Policy)
        
        others_Policy = exp(player$data$policy[[gmboard]][others])/denominator_Policy
        
        others_Grad = player$data$policy[[gmboard]][others] - global_alpha * (reward - avg_reward[others]) * others_Policy
        
        player$data$policy[[gmboard]][[p_action]] = p_action_Grad
        
        player$data$policy[[gmboard]][others] = others_Grad
      }
      
      player$data$episode = list()
    }
    
    action
  }
}

MCEpsSoftAction = function(gameboard, prev_action, point, player, lost = FALSE, draw = FALSE){
  gm = toString(gameboard)
  if (lost) # You lost
  {
    for (gmboard in names(player$data$episode)) 
    {
      p_action = player$data$episode[[gmboard]]
      player$data$return[[gmboard]][[p_action]] = player$data$return[[gmboard]][[p_action]] + 
        (1/player$data$n_actions[[gmboard]][[p_action]]) * (-1 - player$data$return[[gmboard]][[p_action]])

      player$data$policy[[gmboard]] = which.max(rank(player$data$return[[gmboard]], ties.method = "random"))
      
    }
    
    player$data$episode = list()
  }
  else if (draw) # You lost
  {
    for (gmboard in names(player$data$episode)) 
    {
      p_action = player$data$episode[[gmboard]]
      player$data$return[[gmboard]][[p_action]] = player$data$return[[gmboard]][[p_action]] + 
        (1/player$data$n_actions[[gmboard]][[p_action]]) * (0 - player$data$return[[gmboard]][[p_action]])
      #player$data$n_actions[[gmboard]][[p_action]]
      player$data$policy[[gmboard]] = which.max(rank(player$data$return[[gmboard]], ties.method = "random"))
      
    }
    
    player$data$episode = list()
  }
  else # Make a move
  {
    if (is.null(player$data$n_actions[[gm]]))
    {
      player$data$n_actions[[gm]] = rep(0,9)
    }
    if (is.null(player$data$return[[gm]]))
    {
      player$data$return[[gm]] = rep(0,9)
    }
    if (is.null(player$data$policy[[gm]]))
    {
      player$data$policy[[gm]] = 0
    }
    actionSet = extractActionSet(gameboard)
    ## Take the most profitable action or a random one
    action = 0
    p = runif(1)
    epsilon = global_epsilon
    ## Determine ACTION
    if (p < epsilon)
    {
      #actionSet must be larger than 1
      if (length(actionSet) > 1) 
      {
        action = sample(actionSet, 1)
      }
      else 
      {
        action = actionSet[1]
      }
    }
    else 
    {
      action = player$data$policy[[gm]]
      temp_table = player$data$return[[gm]]
      while (!(action %in% actionSet))
        #choose most rewarding action
      {
        if (action != 0)
        {
          temp_table[[action]] = -1000000L
        }
        action = which.max(rank(temp_table, ties.method = "random"))
      }
    }
    ##PLAY THE ACTION
    new_gameboard = gameboard
    new_gameboard[[action]] = point
    player$data$n_actions[[gm]][[action]] = player$data$n_actions[[gm]][[action]] + 1
    
    player$data$episode[[gm]] = action 
    ## Check if you won with that action
    if (checkVictory(new_gameboard) == point)
    {
      ## The less weight we give to a win, in comparison to a loss, the more we win and the less we lose.
      # In this sense, we play more safely when we increase the difference between the weights,
      # however we also get more draws
      for (gmboard in names(player$data$episode)) 
      {
        p_action = player$data$episode[[gmboard]]
        player$data$return[[gmboard]][[p_action]] = player$data$return[[gmboard]][[p_action]] + 
          (1/player$data$n_actions[[gmboard]][[p_action]]) * (1 - player$data$return[[gmboard]][[p_action]])
        player$data$policy[[gmboard]] = which.max(rank(player$data$return[[gmboard]], ties.method = "random"))
      }
      
      player$data$episode = list()
    }
    
    action
  }
}

UCBAction = function(gameboard, prev_action, point, player, lost = FALSE, draw = FALSE){
  if (lost) # You lost
  {
    player$data$return[[prev_action]] = player$data$return[[prev_action]] + 
      (1/5) * (-1 - player$data$return[[prev_action]])
    
  }
  else if (draw) 
  {
    # Do nothing
  }
  else # Make a move
  {
    actionSet = extractActionSet(gameboard)
    ## Take the most profitable action or a random one
    action = 0
    c = 0.10
    maximizing = which(player$data$n_actions == 0)
    temp_return = player$data$return
    temp_return[maximizing] = 1000000L
    temp_n_actions = player$data$n_actions
    temp_n_actions[maximizing] = 1
    t = sum(temp_n_actions)
    options = temp_return + c*sqrt(log(t)/temp_n_actions)
    while (!(action %in% actionSet))
      #choose most rewarding action
    {
      if (action != 0)
      {
        options[[action]] = -1000000L
      }
      action = which.max(rank(options, ties.method = "random"))
    }
    ##PLAY THE ACTION
    new_gameboard = gameboard
    new_gameboard[[action]] = point
    player$data$n_actions[[action]] = player$data$n_actions[[action]] + 1
    ## Check if you won with that action
    if (checkVictory(new_gameboard) == point)
    {
      ## The less weight we give to a win, in comparison to a loss, the more we win and the less we lose.
      # In this sense, we play more safely when we increase the difference between the weights,
      # however we also get more draws
      player$data$return[[action]] = player$data$return[[action]] + 
        (1/5) * (1 - player$data$return[[action]])
    }
    else 
    {
      player$data$return[[action]] = player$data$return[[action]] + 
        (1/5) * (0 - player$data$return[[action]])
    }
    action
  }
}

leftPlayerAction = function(gameboard, prev_action, point, player, lost = FALSE, draw = FALSE){
  actionSet = extractActionSet(gameboard)
  action = min(actionSet)
  action
}

randomPlayerAction = function(gameboard, prev_action, point, player, lost = FALSE, draw = FALSE){
  actionSet = extractActionSet(gameboard)
  if (length(actionSet) > 1) 
  {
    action = sample(actionSet, 1)
  }
  else 
  {
    action = actionSet[1]
  }
  action
}

gradAction = function(gameboard, prev_action, point, player, lost = FALSE, draw = FALSE){
  actionSet = extractActionSet(gameboard)
  action = min(actionSet)
  action
}
##
# MAKE PLAYERS 
##
newPlayer=function(inputData,inputAction){  
  object=new.env(parent=globalenv())  
  object$data=inputData  
  object$action=inputAction
  class(object)='pointer'
  
  return(object)  
}  

##Epsilon-greedy data 
epsilon_Return = rep(0,9)
epsilon_N_Actions = rep(0,9)
epsilonData = list(return = epsilon_Return, n_actions = epsilon_N_Actions)

contextualEps_Return = hash(keys=c('-1','0','1',',',' '), values = 1:5)
contextualEps_N_Actions = hash(keys=c('-1','0','1',',',' '), values = 1:5)
contextualEpsData = list(return = contextualEps_Return, 
                             n_actions = contextualEps_N_Actions)

MCEpsSoft_Return = hash(keys=c('-1','0','1',',',' '), values = 1:5)
MCEpsSoft_N_Actions = hash(keys=c('-1','0','1',',',' '), values = 1:5)
MCEpsSoft_Episode = list()
MCEpsSoft_Policy = hash(keys=c('-1','0','1',',',' '), values = 1:5)
MCEpsSoftData = list(return = MCEpsSoft_Return, 
                         n_actions = MCEpsSoft_N_Actions,
                         episode = MCEpsSoft_Episode,
                         policy = MCEpsSoft_Policy)

MCEpsSoft_Return2 = hash(keys=c('-1','0','1',',',' '), values = 1:5)
MCEpsSoft_N_Actions2 = hash(keys=c('-1','0','1',',',' '), values = 1:5)
MCEpsSoft_Episode2 = list()
MCEpsSoft_Policy2 = hash(keys=c('-1','0','1',',',' '), values = 1:5)
MCEpsSoftData2 = list(return = MCEpsSoft_Return2, 
                     n_actions = MCEpsSoft_N_Actions2,
                     episode = MCEpsSoft_Episode2,
                     policy = MCEpsSoft_Policy2)

RegularGradAsc_Return = hash(keys=c('-1','0','1',',',' '), values = 1:5)
RegularGradAsc_N_Actions = hash(keys=c('-1','0','1',',',' '), values = 1:5)
RegularGradAsc_Episode = list()
RegularGradAsc_Policy = hash(keys=c('-1','0','1',',',' '), values = 1:5)
RegularGradAscData = list(return = RegularGradAsc_Return, 
                      n_actions = RegularGradAsc_N_Actions,
                      episode = RegularGradAsc_Episode,
                      policy = RegularGradAsc_Policy)


contextualUCB_Return = hash(keys=c('-1','0','1',',',' '), values = 1:5)
contextualUCB_N_Actions = hash(keys=c('-1','0','1',',',' '), values = 1:5)
contextualUCBData = list(return = contextualUCB_Return, 
                            n_actions = contextualUCB_N_Actions)

resetData=function(data)
{
  clear(data$return)
  clear(data$n_actions)
}

resetMCData=function(data)
{
  clear(data$return)
  clear(data$n_actions)
  clear(data$policy)
  data$episode = list()
}


UCB_Return = rep(0,9)
UCB_N_Actions = rep(0,9)
UCBData = list(return = UCB_Return, n_actions = UCB_N_Actions)

##hash table for game board states
epsData2 = hash(keys=c('-1','0','1',',',' '), values = 1:5)
emptyGameboard = toString(rep(0,9))
epsData2[[emptyGameboard]] = rep(0,9)

sLearn = hash(keys=c('-1','0','1',',',' '), values = 1:5)
gLearn = hash(keys=c('-1','0','1',',',' '), values = 1:5)
hLearn  = list(n_games = 0, result = list(), games = list())


playerRandom = newPlayer(NULL,randomPlayerAction)
playerLeft = newPlayer(NULL,leftPlayerAction)
playerEpsImproved = newPlayer(epsData2,epsGreedyActionImproved)
playerGrad = newPlayer(list(strategyLearn = sLearn, 
                            gradientLearn = gLearn, 
                            historyLearn = hLearn),gradAction)
##
# RUN AND PLOT GAMES
##


playGames = function(n_games, interval_of_winrate = 100, first_player, second_player, p_switch = FALSE)
{
  global_ngames <<- 0
  ChangedConditions <<- FALSE
  n = as.numeric(n_games)
  div = as.numeric(interval_of_winrate)
  total_wins = 0
  total_draws = 0
  total_loss = 0
  wins = rep(0,n)
  losses = rep(0,n)
  draws = rep(0,n)
  winrate = rep(0,n/div)
  for(i in 1:n)
  {
    global_ngames <<- global_ngames + 1
    newStats = game(first_player,second_player, switch = p_switch)
    
    total_draws = total_draws + newStats[1]
    total_wins = total_wins + newStats[2]
    total_loss = total_loss + newStats[3]
    
    draws[i] = total_draws
    wins[i] = total_wins
    losses[i] = total_loss
    if (i %% div == 0)
    {
      winrate[i/div] = wins[i]/i
    }
  }
  list(wins=wins,losses=losses,draws=draws,winrate=winrate)
}

# Visualize the optimal strategies against the left-player and random player for the first two moves
# 
                         