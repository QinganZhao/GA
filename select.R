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
#source('foo.R')
#...(list all the sources)

#this is the primary function of the R package of this project
#the output of this function should be ...(to be discussed)

select <- function(X, Y, popNum = 100, foo, zeroRate = 0.5){
  
  #X is treated as a data frame; Y is treated as a vector
  X <- as.data.frame(X)
  Y <- as.vector(Y)
  
  if(dim(X)[1] != length(Y)){
    stop("Invalid data: X and Y does not match")
  }
  
  #intialization: generate the first generation
  geneLength <- dim(X)[2]
  firstGeneration <- popInitial(popNum, geneLength, zeroRate)
  currentGeneration <- firstGeneration
  
  #first evaluation
  firstEval <- evaluation(X, Y, firstGeneration, popNum, reg, criterion)
  
  #first rank
  firstRank <- rank_selection(firstEval, usingrank)
  
  #iterations for the GA algorithm
  i <- 1
  max_iter <- 1e6 #max iteration: to be discussed
  tol <- 1e-6 #tolerance: to be discussed
  
  for(i in 1:max_iter & error > tol){
    ## Mia is working on this part. Tommorrow we are going to discuss it.
  }
  
  return(model)
}
