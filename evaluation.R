# This function has the following inputs:
# X (user given matrix), Y (user given vector)
# currentGeneration: the initialized generation and the selectively produced generations
# popNum: size of the population for chromosomes, default as 100
# geneLength
# reg: regression type, lm as default
# criterion: objective criterion function, AIC as default

# The output dataframe is popNum by 2, with the first column consists of chromosomes and the second column consists of fitness values

evaluation <- function(X, Y, currentGeneration, popNum = 100, reg = 'lm', criterion = 'AIC'){
  # construct the outcome dataframe: popNum by 2
  # column 1: ranking of chromosome, column 2: AIC fitness value
  df_init <- as.data.frame(matrix(nrow = popNum, ncol = 2))
  colnames(df_init) <- c("chromosome","fitness")
  df_init$chromosome <- apply(currentGeneration, 1, paste, collapse = '') # pasting chromosomes as strings
  
  # default setting: regression reg = lm
  if(reg == 'lm'){
    # criterion is AIC
    if(criterion == 'AIC'){
      for(i in 1:popNum){
        df_init$fitness[i] <- AIC(lm(Y~as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
      }
    }
    # criterion is another function provided by the user
    else{
      for(i in 1:popNum){
        df_init$fitness[i] <- eval(criterion, lm(Y ~ as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
      }
    }
  }

  # regression type = glm
  else if(reg == 'glm'){
    # criterion is AIC
    if(criterion == 'AIC'){
      for(i in 1:popNum){
        df_init$fitness[i] <- AIC(glm(Y~as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
      }
    }
    # criterion is another function provided by the user
    else{
      for(i in 1:popNum){
        df_init$fitness[i] <- eval(criterion, lm(Y ~ as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
      }
    }
  }
  
  else{
    stop("Regression type must be 'lm' or 'glm'.")
  }

  return(df_init)
}




