
# script for the mutation
# the input should be a list with one column corresponding to the choromosomes
# user can choose the probability of mutation. Default is 0.01
# Output is a list with one column corresponding to the choromosomes
# the main function that other parts of the project should use is mutation

#LIBRARIES:
library(dplyr)


#FUNCTIONS:

doMutation <- function(chromosome, probability = 0.01)
{
  pVector <- runif(length(chromosome))
  chromosome <- (1-chromosome)*(pVector<=probability)+(chromosome)*(pVector>probability)
  return(chromosome)
}

mutation <- function(list_init, p = 0.01) 
{
  mutatedList <- lapply(list_init, doMutation, probability = p)
  return(mutatedList)
}
