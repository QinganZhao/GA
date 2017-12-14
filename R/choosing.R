
# script for the choosing (it is selecting chromosomes but I call it choosing to not get confused with the selection!) chromosomes
# the input should be a dataframe with one column corresponding to the choromosomes, and one to the probability of getting chosed for each chromosome
# 2 options are available: choosing both chromosome based on their rank (rankBased = TRUE) or choosing one of them uniformly.
# Output is a dataframe with two columns. Both column corresponding to chromosomes. Each row of this data frame shows both parents.



#FUNCTIONS:

#' support function: chooseChromosomes()
#'
#' This function put choose/select the chromosomes for generating the next generation. 
#' @param df_init data frame; ranked chromosomes
#' @param rankBased logic; TRUE is choosing both parent based on their fitness and FALSE is choosing one parent based on fitness and the other one randomly; defaults to TRUE;
#' @keywords chooseChromosomes
#' @export
#' @examples
#' chooseChromosomes()
chooseChromosomes <- function(df_init, rankBased = TRUE) 
{
  
  #number of chromosomes
  if (length(df_init$chromosome)%%2 == 1)
  {
    print("The population number is ODD. It should be EVEN.")
  }
  pop <- as.integer(length(df_init$chromosome)/2)
  
  
  #sampling chromosomes
  df1<-dplyr::sample_n(df_init, size = pop, weight = df_init$probability, replace = TRUE)
  if (rankBased == TRUE)
  {
    df2<-dplyr::sample_n(df_init, size = pop, weight = df_init$probability, replace = TRUE)
  }else
  {
    df2<-dplyr::sample_n(df_init, size = pop, replace = TRUE)
  }
  
  #create the output dataframe
  df <- data.frame(matrix(ncol = 2, nrow = pop))
  colnames(df) <- c("chromosomeSet1","chromosomeSet2")
  df$chromosomeSet1 <- df1$chromosome
  df$chromosomeSet2 <- df2$chromosome
  
  return(df)
}
