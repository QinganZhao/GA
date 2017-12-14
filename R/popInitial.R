##### support function: popInitial() #####

# this function intializes the population


#' support function: popInitial()
#'
#' This function intializes the population. 
#' @param popNum integer; What is the population size of chromosomes? Default to 100
#' @param geneLength integer; number of genes (0/1) on a chromosome
#' @param zeroRate numeric; below or equal to 1 and above or equal to 0; What is the rate of zero among the genes of the first generation? Defaults to 0.5
#' @keywords popInitial
#' @export
#' @examples
#' popInitial()
popInitial <- function(popNum = 100, geneLength, zeroRate = 0.5){
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
  
  
  return(firstGeneration)
}
