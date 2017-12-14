# This function has the following inputs:
# X (user given matrix), Y (user given vector)
# currentGeneration: the initialized generation and the selectively produced generations
# popNum: size of the population for chromosomes, default as 100
# geneLength
# reg: regression type, lm as default
# criterion: objective criterion function, AIC as default

# The output dataframe is popNum by 2, with the first column consists of chromosomes and the second column consists of fitness values



#' support function: evaluation()
#'
#' This function evaluate the fitness value of chromosomes. 
#' @param X matrix/dataframe; predictors
#' @param Y vector/matrix/dataframe; observations to be used in the fitting process
#' @param currentGeneration matrix; selectively produced generations
#' @param popNum integer; What is the population size of chromosomes? Default to 100
#' @param reg string; 'lm'/'glm; What is the regression type? Default to 'lm'
#' @param criterion string; function name; What is the objective criterion function? Default to 'AIC'
#' @keywords evaluation
#' @export
#' @examples
#' evaluation()
evaluation <- function(X, Y, currentGeneration, popNum = 100, reg = 'lm', criterion = 'AIC'){
  
  # check if X and Y has NAs
  if(is.na(sum(X)) | is.na(sum(Y))){
    warning("There are NAs in X or Y matrix. The default setting of lm and glm function uses 'na.omit' that omits the observations with NAs.")
  }
  #if a population is identically 0, we force a 'mutation' by change a random gene to 1
  #since this case cannot pass regression
  currentGeneration[which(apply(currentGeneration, 1, sum) == 0), 
                  sample(1:ncol(currentGeneration), 1)] <- 1
  
  # construct the outcome dataframe: popNum by 2
  # column 1: ranking of chromosome, column 2: AIC fitness value
  df_current <- as.data.frame(matrix(nrow = popNum, ncol = 2))
  colnames(df_current) <- c("chromosome","fitness")
  df_current$chromosome <- apply(currentGeneration, 1, paste, collapse = '') # pasting chromosomes as strings
  
  # default setting: regression reg = lm
  if(reg == 'lm'){
    # criterion is AIC
    if(criterion == 'AIC'){
      for(i in 1:popNum){
        df_current$fitness[i] <- AIC(lm(Y~as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
        if(is.infinite(df_current$fitness[i])){
          warning("There is an infinite AIC (might be overfitting).")
        }
      }
    }else{ # criterion is another function provided by the user
      for(i in 1:popNum){
        df_current$fitness[i] <- eval(criterion, lm(Y ~ as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
      }
    }
  }

  # regression type = glm
  else if(reg == 'glm'){
    # criterion is AIC
    if(criterion == 'AIC'){
      for(i in 1:popNum){
        df_current$fitness[i] <- AIC(glm(Y~as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
        if(is.infinite(df_current$fitness[i])){
          warning("There is an infinite AIC (might be overfitting).")
        }
      }
    }
    # criterion is another function provided by the user
    else{
      for(i in 1:popNum){
        df_current$fitness[i] <- eval(criterion, lm(Y ~ as.matrix(X[,which(currentGeneration[i,] == 1, arr.ind = T)])))
      }
    }
  }
  
  else{
    stop("Regression type must be 'lm' or 'glm'.")
  }

  return(df_current)
}


