## Teammates:
## Please add all your scripts with your functions by replace "foo.R" by your scripts
## or we could combine all our functions into one script and add it to the source
## please make sure the names of our functions are not duplicate 

source('popInitial.R')
#source('foo.R')
#...(list all the sources)

#this is the primary function of the R package of this project
#the output of this function should be 

## Teammates:
## please add the arguments of all of your functions here (replace 'foo')
## please try to place the less important argument with default values behind 
## (such as the 'zeroRate' in my popInitial() function)
select <- function(X, Y, popNum = 100, foo, zeroRate = 0){
  
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
  
  ## Mia: Here we should call your function to do the first evaluation
  ## please change/add/delete the arguments and/or the function name
  ## type: lm (by default) or glm
  ## criterion: AIC (by default) or other fitness function that the user provides 
  firstEval <- evaluation(X, Y, currentGeneration, popNum, geneLength, type, criterion, ...)
  
  ## Arman or Côme: (sorry I forgot which one of you is working on this function)
  ## Here we should rank the first evaluation
  ## please change/add/delete the arguments and/or the function name
  firstRank <- rankPop(firstEval, ...)
  
  #iterations for the GA algorithm
  i <- 1
  max_iter <- 1e6 #max iteration: to be discussed
  tol <- 1e-6 #tolerance: to be discussed
  
  for(i in 1:max_iter & error > tol){
    #this part will be completed when I check all of your functions
  }
  
  return(model)
}