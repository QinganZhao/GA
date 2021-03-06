#### IMPORTANT: Please make sure you have installed 'dplyr' package ####

## Please make sure you have installed the following packages if you choose parallelization ##
## parallel, doParallel, & foreach

########################################################################


#this is the primary function of the R package of this project
#the output of this function should be the selected predictors 



#' support function: select()
#'
#' This is the primary function of the R package of this project. 
#' @param X matrix/dataframe; predictors
#' @param Y vector/matrix/dataframe; observations to be used in the fitting process
#' @param popNum integer; What is the population size of chromosomes? Default to 100
#' @param reg string; 'lm'/'glm; What is the regression type? Default to 'lm'
#' @param criterion string; function name; What is the objective criterion function? Default to 'AIC'
#' @param useParallel logic; Should it use parallelization? Default to FALSE
#' @param numCores integer; How many cores should it use? Default to 4
#' @param usingRank logic; TRUE define selection probabilities using the rank and FALSE create selection probabilities proportionate to fitness; defaults to TRUE
#' @param cross_cutNum integer; How many cuts do you want for the crossover? Defaults to 1
#' @param mutation_prob numeric; below or equal to 1 and above or equal to 0; What is the probability of mutation? Defaults to 0.01
#' @param mut_pCurve logic; Should it use curve for the probability of mutation (Starting with a big probability and decreasing to mutation_prob by the time)? Defaults to FALSE.
#' @param initial_zeroRate numeric; below or equal to 1 and above or equal to 0; What is the rate of zero among the genes of the first generation? Defaults to 0.5
#' @param min_iter integer; What is the minimum number of iterations? Defaults to 10.
#' @param max_iter integer; What is the maximum number of iterations? Defaults to 500.
#' @keywords select
#' @export
#' @examples
#' select()
select <- function(X, Y, popNum = 100, reg = 'lm', criterion = 'AIC', useParallel = FALSE,
                   numCores = 4, usingRank = TRUE, cross_cutNum = 1, mutation_prob = 0.01,
                   mut_pCurve = FALSE, initial_zeroRate = 0.5, min_iter = 10, max_iter = 500){
  
  #support scripts (functions)
  #source('popInitial.R')
  #source('evaluation.R')
  #source('rankSelection.R')
  #source('choosing.R')
  #source('crossover.R')
  #source('mutation.R')
  #source('breed.R')
  
  #prepare for parallelization if the user choose it
  if(useParallel == TRUE){
    library(parallel)
    library(doParallel)
    library(foreach)
    nCores <- numCores
    registerDoParallel(nCores)
    #source('superEvaluation.R')
  }
  
  #X is treated as a data frame; Y is treated as a vector
  X <- as.data.frame(X)
  Y <- as.vector(Y)
  
  if(dim(X)[1] != length(Y)){
    stop("Invalid data: X and Y do not match")
  }
  if (min_iter>max_iter) {
    stop("the maximum number of iteration
         must be bigger than the minimum number of iteration")
  }
  
  #intialization: generate the first generation
  geneLength <- dim(X)[2]
  firstGeneration <- popInitial(popNum, geneLength, zeroRate = initial_zeroRate)
  currentGeneration <- firstGeneration
  
  #first evaluation
  if(useParallel == TRUE){
    df_current <- superEvaluation(X, Y, firstGeneration, popNum, reg, criterion)
  }else{
    df_current <- evaluation(X, Y, firstGeneration, popNum, reg, criterion)
  }
  
  #iterations for the GA
  #this can go right after the evaluation as well
  i <- 1
  threshold <- 0
  dif <- 0
  fit_record <- c(min(df_current$fitness))
  mutation_prob_backup <- mutation_prob
  
  while(i <= min_iter | (i > min_iter & i <= max_iter & dif > threshold)){
 
	# curving the mutation probability
	  if(mut_pCurve == TRUE){
		  mutation_prob <- min(mutation_prob_backup + 1/(i+1),1)
	  }
    
    if(i > min_iter){
      dif <- abs(min(df_current$fitness) - mean(fit_record[(i - min_iter + 1):i]))
      threshold <- .Machine$double.eps * abs(min(df_current$fitness) + mean(fit_record[(i - min_iter):i]))
    }
    
    #implement GA   
    crtRank <- rankSelection(df_current, usingRank)
    crtChosen <- chooseChromosomes(crtRank, usingRank)
    nextGeneration <- breed(crtChosen, cross_cutNum, mutation_prob)
    
    if(useParallel == TRUE){
      df_current <- superEvaluation(X, Y, nextGeneration, popNum, reg, criterion)
    }else{
      df_current <- evaluation(X, Y, nextGeneration, popNum, reg, criterion)
    }
      
	  fit_record <- c(fit_record, min(df_current$fitness))
	  i <- i + 1
  }
  
  #the selected predictors
  predictor_tmp <- unlist(strsplit(df_current[1,1:(dim(df_current)[2]-1)], NULL))
  predictor. <- as.matrix(X[which(predictor_tmp == '1')])
  
  #return the best model
  if(reg == 'lm'){
    bestModel <- lm(Y ~ predictor.)
  }else{
    bestModel <- glm(Y ~ predictor.)
  }
  
  cat(' Selected predictors:', colnames(predictor.), '\n', 'Fitness value:', 
      mean(fit_record),'\n')
  
  return(bestModel)
}
