
# script for the mutation
# the input should be a list with one column corresponding to the choromosomes
# user can choose the probability of mutation. Default is 0.01
# Output is a list with one column corresponding to the choromosomes
# the main function that other parts of the project should use is mutation




#FUNCTIONS:
#' support function: mutation()
#'
#' This function mutate the chromosomes. 
#' @param chromosome vector; one chromosome with genes 0/1
#' @param probability numeric; below or equal to 1 and above or equal to 0; What is the probability of mutation? Defaults to 0.01
#' @keywords mutation
#' @export
#' @examples
#' mutation()
mutation <- function(chromosome, probability = 0.01)
{
  pVector <- runif(length(chromosome))
  chromosome <- (1-chromosome)*(pVector<=probability)+(chromosome)*(pVector>probability)
  return(chromosome)
}

