#script for the ranking function
#the input should be a database with one column corresponding to the , and one to the AIC values of each chromosome
# 2 options are available: using the rank of the chromosome (default) or its fitness.

rank_selection <- function(df_init,usingrank = TRUE) {
  #number of chromosomes
  pop <- length(df_init$chromosome)
  
  if (is.na(df_init)) {return("The are N/A values in the dataframe")}
  
  #create the output dataframe
  df <- data.frame(matrix(ncol = 2, nrow = pop))
  colnames(df) <- c("chromosome","probability")
  
  #order the chromosomes by fitness
  reorder <- order(df_init$fitness, decreasing = TRUE)
  df$chromosome <- df_init$chromosome[reorder]
  
  #default case: define selection probabilities using the rank
  if (usingrank) {
    for (i in 1:pop) {
      df$probability[i] <- (pop+1-i)*2/(pop*(pop+1))
      #this formula, suggested by Givens
      #assigns decreasing probabilities depending on the rank
    }
    
  } else {
#alternate case: create selection probabilities proportionate to fitness
    sum_fit = sum(df_init$fitness)
    df$probability <- df_init$fitness[reorder] / sum_fit
  }
  return(df)
}
