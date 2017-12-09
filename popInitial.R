##### support function: popInitial() #####

# this function intializes the population
popInitial <- function(popNum = 100, geneLength, zeroRate = 0){
  #input: 
  #   popNum: population number (positive integer)
  #   geneLength: gene length (positive integer)
  #   zeroRate: the rate of zero among the genes of the first generation
  #output: populations of the first generation with their genes (0/1 matrix)
  
  if(popNum < 1 | round(popNum) != popNum){
    stop("Population number must be a positive integer.")
  }
  if(geneLength < 1 | round(geneLength) != geneLength){
    stop("Gene length must be a positive integer.")
  }
  if(zeroRate <= 0 | zeroRate >= 1){
    stop("Zero rate must be within (0,1).")
  }
  
  #determine the numbers of zeros and ones based on the zeroRate
  geneNum <- popNum * geneLength
  zeroNum <- round(geneNum * zeroRate)
  oneNum <- geneNum - zeroNum 
  
  #generate the first generation
  firstGeneration <- matrix(sample(c(rep(0, zeroNum), rep(1, oneNum)), geneNum), 
                            nrow = popNum, ncol = geneLength)
  
  #if a population is identically 0, we force a 'mutation' by change a random gene to 1
  #since this case cannot pass evaluation()
  firstGeneration[which(apply(firstGeneration, 1, sum) == 0), 
                    sample(1:ncol(firstGeneration), 1)] <- 1
  
  return(firstGeneration)
}
