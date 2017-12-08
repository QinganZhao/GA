## Teammates:
## Please add all your scripts with your functions by replace "foo.R" by your scripts
## or we could combine all our functions into one script and add it to the source
## please make sure the names of our functions are not duplicate 

source('popInitial.R')
source('evaluation.R')
source('rank_selection.R')
source('choosing.R')
source('crossover.R')
source('mutation.R')
source('breed.R')
#source('foo.R')
#...(list all the sources)

#this is the primary function of the R package of this project
#the output of this function should be ...(to be discussed)

select <- function(X, Y, popNum = 100, reg = 'lm', criterion = 'AIC', usingrank = TRUE,
                   choose_rankBased = TRUE, cross_cutNum = 1, mutation_prob = 0.01,
                   initial_zeroRate = 0.5, t_last = 15, t_max = 500){
  
  #X is treated as a data frame; Y is treated as a vector
  X <- as.data.frame(X)
  Y <- as.vector(Y)
  
  if(dim(X)[1] != length(Y)){
    stop("Invalid data: X and Y does not match")
  }
  
  #intialization: generate the first generation
  geneLength <- dim(X)[2]
  firstGeneration <- popInitial(popNum, geneLength, zeroRate = initial_zeroRate)
  currentGeneration <- firstGeneration
  
  #first evaluation
  firstEval <- evaluation(X, Y, firstGeneration, popNum, reg, criterion)
  
  #first rank
  firstRank <- rank_selection(firstEval, usingrank)
  
  #test part (will be deleted later)
  foo1 <- chooseChromosomes(firstRank, rankBased = choose_rankBased)
  foo2 <- breed(foo1, cross_cutNum, mutation_prob)
  
  #iterations for the GA algorithm (and termination)
  # This can go right after the evaluation as well.
  t = 1
  AIC_record <- c(min(firstEval$fitness))
  
  while(t <= t_last | (t > t_last & t <= t_max & approxEqual(min(df_current$fitness), mean(AIC_record[(t - t_last + 1):t])) = FALSE)){
  # OR while(t <= t_last | (t > t_last & t <= t_max & converge(df_current, cvgRate_allele = 0.9, cvgRate_chrom = 0.9) = FALSE)
    crtChosen <- choosing(firstRank)
    nextGeneration <- breed(crtChosen)
    df_current <- evaluation(nextGeneration)
    t = t + 1
    AIC_record <- c(AIC_record, min(df_current$fitness))
  }
  
  # the returned model
  predictors <- df_current[1,1:(dim(df_current)[2]-1)]
  # do we need to write out the regression function?
  # selected_model <-

  return(selected_model)
}
