#script for the ranking function
#the input should be a database with one column corresponding to the , and one to the AIC values of each chromosome
# 2 options are available: using the rank of the chromosome (default) or its fitness.


#' support function: rankSelection()
#'
#' This function create selection probabilities of the chromosomes. 
#' @param dfCurrent dataframe; one column corresponding to the genes (string), and one to the AIC values of each chromosome
#' @param usingRank logic; TRUE define selection probabilities using the rank and FALSE create selection probabilities proportionate to fitness; defaults to TRUE 
#' @keywords rankSelection
#' @export
#' @examples
#' rankSelection()
rankSelection <- function(dfCurrent,usingRank = TRUE) {
  #number of chromosomes
  pop <- length(dfCurrent[[1]])
  
  if (is.na(dfCurrent[[1]][sample(1:pop, size = 1)])) {
    stop("The are N/A values in the dataframe")
  }
  
  #create the output dataframe
  df <- data.frame(matrix(ncol = 2, nrow = pop))
  colnames(df) <- c("chromosome","probability")
  
  #order the chromosomes by fitness
  reorder <- order(dfCurrent[[2]], decreasing = FALSE)
  df$chromosome <- dfCurrent[[1]][reorder]
  
  #default case: define selection probabilities using the rank
  if (usingRank) {
    for (i in 1:pop) {
      df$probability[i] <- (pop+1-i)*2/(pop*(pop+1))
      #this formula, suggested by Givens
      #assigns decreasing probabilities depending on the rank
    }
    
  } else {
#alternate case: create selection probabilities proportionate to fitness
#excludes the less fit from the selection
    tmp <- dfCurrent[[2]][reorder]
    if (sum(is.infinite(tmp))>0) { stop("some of the fitness values are infinite, choose usingRank=TRUE")}
    tmp <- -(tmp - max(tmp))
    sumFit = sum(tmp)
    df$probability <- tmp / sumFit
  }
  return(df)
}
