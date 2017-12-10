# This function is the parallelization version of evaluation()

superEvaluation <- function(X, Y, currentGeneration, popNum = 100, reg = 'lm', criterion = 'AIC'){
  
  # check if X and Y has NAs
  if(is.na(sum(X)) | is.na(sum(Y))){
    warning("There are NAs in X or Y matrix. The default setting of lm and glm function uses 'na.omit' that omits the observations with NAs.")
  }
  
  # construct the outcome dataframe: popNum by 2
  # column 1: ranking of chromosome, column 2: AIC fitness value
  df_current <- as.data.frame(matrix(nrow = popNum, ncol = 2))
  colnames(df_current) <- c("chromosome","fitness")
  df_current$chromosome <- apply(currentGeneration, 1, paste, collapse = '') # pasting chromosomes as strings
  
  # default setting: regression reg = lm
  if(reg == 'lm'){
    # criterion is AIC
    if(criterion == 'AIC'){
      
      #parallelization
      result <- foreach(i = 1:popNum, .combine = rbind)  %dopar% {
        fitness <- AIC(lm(Y~as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
        return(fitness)
      }
      
    }else{ # criterion is another function provided by the user
      
      #parallelization
      result <- foreach(i = 1:popNum, .combine = rbind)  %dopar% {
        fitness <- eval(criterion, lm(Y ~ as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
        return(fitness)
      }
      
    }
  }
  
  # regression type = glm
  else if(reg == 'glm'){
    # criterion is AIC
    if(criterion == 'AIC'){
      
      #parallelization
      result <- foreach(i = 1:popNum, .combine = rbind)  %dopar% {
        fitness <- AIC(glm(Y~as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
        return(fitness)
      }
      
    }
    # criterion is another function provided by the user
    else{
      
      #parallelization
      result <- foreach(i = 1:popNum, .combine = rbind)  %dopar% {
        fitness <- eval(criterion, glm(Y ~ as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
        return(fitness)
      }
      
    }
  }
  
  else{
    stop("Regression type must be 'lm' or 'glm'.")
  }
  
  #put the fitness into the data frame 
  df_current$fitness <- result

  return(df_current)
}

