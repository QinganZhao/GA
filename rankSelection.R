#script for the ranking function
#the input should be a database with one column corresponding to the , and one to the AIC values of each chromosome
# 2 options are available: using the rank of the chromosome (default) or its fitness.

rankSelection <- function(dfCurrent,usingrank = TRUE) {
  #number of chromosomes
  pop <- length(dfCurrent$chromosome)
  
  if (is.na(dfCurrent[[1]][sample(1:pop, size = 1)])) {
    stop("The are N/A values in the dataframe")
  }
  
  #create the output dataframe
  df <- data.frame(matrix(ncol = 2, nrow = pop))
  colnames(df) <- c("chromosome","probability")
  
  #order the chromosomes by fitness
  reorder <- order(dfCurrent$fitness, decreasing = FALSE)
  df$chromosome <- dfCurrent$chromosome[reorder]
  
  #default case: define selection probabilities using the rank
  if (usingrank) {
    for (i in 1:pop) {
      df$probability[i] <- (pop+1-i)*2/(pop*(pop+1))
      #this formula, suggested by Givens
      #assigns decreasing probabilities depending on the rank
    }
    
  } else {
#alternate case: create selection probabilities proportionate to fitness
#excludes the less fit from the selection
    tmp <- dfCurrent$fitness[reorder]
    if (sum(is.infinite(tmp))>0) { stop("some of the fitness values are infinite, choose usingrank=TRUE")}
    tmp <- -(tmp - max(tmp))
    sumFit = sum(tmp)
    df$probability <- tmp / sumFit
  }
  return(df)
}
