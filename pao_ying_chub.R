## Homework - Pao Ying Chub

pao_ying_chub <- function(){
  #variables
  current_round <- 1
  user_score <- 0
  com_score <- 0
  
  # print game rules
  cat("Welcome to Pao Ying Chub Game!\n")
  game_round <- as.numeric(readline("How many round that you want to play?: "))
  
  while(game_round <= 0){
    cat("Please enter a number at least 1 round to play this game!\n")
    game_round <- as.numeric(readline("How many round that you want to play?: "))
  } 
  cat("Okee! Game Starts!")
  df <- matrix(data = NA, nrow = game_round, ncol = 3, byrow = TRUE)
  df <- as.data.frame(df)
  colnames(df) <- c("You", "Com", "Outcome")
  hands <- c("Rock", "Paper", "Scissors")

  while(game_round >= current_round){
    user_hand <- as.numeric(readline("Please select hand [1:Rock, 2:Paper, 3:Scissors]: "))
      while(user_hand != 1 & user_hand != 2 & user_hand != 3){
        cat("Please enter only [1:Rock, 2:Paper, 3:Scissors] ! ")
        user_hand <- as.numeric(readline("Please select hand [1:Rock, 2:Paper, 3:Scissors]: "))
      }
      com_hand <- sample(hands, 1)
      x <- hands[user_hand]
    
      if(x == com_hand){
        df[current_round, ] <- data.frame(com_hand, com_hand,"Draw")
        }
      else if(hands[user_hand] == "Rock"){
        if(com_hand == "Scissors"){
          df[current_round, ] <- data.frame(hands[user_hand], com_hand,"Win")
          user_score <- user_score + 1
        } else{
          df[current_round, ] <- data.frame(hands[user_hand], com_hand,"Lose")
          com_score <- com_score + 1
        }
      }
      else if(hands[user_hand] == "Paper"){
        if(com_hand == "Rock"){
          df[current_round, ] <- data.frame(hands[user_hand], com_hand,"Win")
          user_score <- user_score + 1
        } else{
          df[current_round, ] <- data.frame(hands[user_hand], com_hand,"Lose")
          com_score <- com_score + 1
        }
      }
      else if(hands[user_hand] == "Scissors"){
        if(com_hand == "Paper"){
          df[current_round, ] <- data.frame(hands[user_hand], com_hand,"Win")
          user_score <- user_score + 1
        } else{
          df[current_round, ] <- data.frame(hands[user_hand], com_hand,"Lose")
          com_score <- com_score + 1
        }
      }
      cat("Result:\n")
      print(df[current_round, ])
      current_round <- current_round + 1
      
    }
  cat("Summary:\n")
  print(df)
  
  if(user_score > com_score){
    text <- "You win!! So luckyyyy!"
  } else if (user_score < com_score){
    text <- "You lose..better try next time!"
  } else{
    text <- "Draw..this bot so lucky! "
  }
  
  print(paste("Finally, Your Score: ", user_score, "| Bot Score: ", com_score, "->", text ))
  
}


    
  


  

