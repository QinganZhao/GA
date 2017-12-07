# This function has the following inputs:
# X (user given matrix), Y (user given vector)
# currentGeneration: the initialized generation and the selectively produced generations
# popNum: size of the population for chromesomes, default as 100
# geneLength
# reg: regression type, lm as default
# criterion: objective criterion function, AIC as default

# The output dataframe is popNum by 2, with the first column consists of chromesomes and the second column consists of fitness values

evaluation <- function(X, Y, currentGeneration, popNum = 100, geneLength, reg = lm, criterion = AIC){
  # construct the outcome dataframe: popNum by 2
  # column 1: ranking of chromesome, column 2: AIC fitness value
  df_init <- as.dataframe(matrix(nrow = popNum, ncol = 2))
  colnames(df_init) <- c("chromesome","fitness")
  df_init$chromesome <- apply(currentGeneration, MARGIN = 1, FUN = paste0()) # pasting chromesomes as strings
  
  # default setting, regression reg = lm, criterion = AIC
  for(i in 1:popNum){
    df_init$fitness[i] <- AIC(lm(Y ~ X[,currentGeneration[i,]==1]))
  }

  # regression type = glm, criterion = AIC
  if(reg = glm){
    for(i in 1:popNum){
      df_init$fitness[i] <- AIC(glm(Y ~ X[,currentGeneration[i,]==1]))
    }
  }
  
  # criterion != AIC
  if(criterion != AIC){
    if(criterion = BIC){
      for(i in 1:popNum){
        df_init$fitness[i] <- BIC(lm(Y ~ X[,currentGeneration[i,]==1]))
      }
     else{
       df_init$fitness[i] <- criterion(lm(Y ~ X[,currentGeneration[i,]==1]))
     } 
    }
  }
  return(df_init)
}




