##### support function: breed() #####

# this function put the crossover and mutation parts together
# aslo aims to make sure the alignment of the data type in each part
# applies the crossover() and mutation() functions to all chromosomes


#' support function: breed()
#'
#' This function put the crossover and mutation parts together. Aslo aims to make sure the alignment of the data type in each part applies the crossover() and mutation() functions to all chromosomes.
#' @param chromoSet dataframe; both column corresponding to chromosomes; Each row of this data frame shows both parents
#' @param cross_cutNum integer; How many cuts do you want for the crossover? Defaults to 1
#' @param mutation_prob numeric; What is the probability of mutation? Defaults to 0.01
#' @keywords breed
#' @export
#' @examples
#' breed()
breed <- function(chromoSet, cross_cutNum = 1, mutation_prob = 0.01){
  #input:
  #   chromoSet: (data frame) 2 sets of chromosomes generated from the choosing() function
  #   cross_cutNum: (positive integer) the 3rd argument of the crossover() function
  #   mutation_prob: (numeric; above or equal to 0 and below or equal to 1)
  #output: populations of the next generation with their genes (0/1 matrix)
  
  if(cross_cutNum < 1 | round(cross_cutNum) != cross_cutNum){
    stop("Cut number of the crossover must be a positive integer.")
  }
  if(mutation_prob < 0 | mutation_prob > 1){
    stop("Mutation probability must be within [0,1].")
  }
  
  #source("crossover.R")
  #source("mutation.R")
  
  chromo_mat <- as.matrix(chromoSet)  
  pop_len <- length(chromo_mat) / 2  #the population number of each group (set)
  #number of genes on each chromosome
  gene_len <- length(unlist(strsplit(chromoSet[[1]][1], NULL))) 
  chromo_list <- list()
  #array for genes to be decomposed
  gene_array <- array(dim = c(2, gene_len, pop_len))  
  cross_tmp <- list()
  chro_for_mut <- matrix(nrow = 2, ncol = gene_len)
  #matrices for each group of the next generation
  child_gene_mat1 <- matrix(nrow = pop_len, ncol = gene_len)  
  child_gene_mat2 <- matrix(nrow = pop_len, ncol = gene_len)
  
  #breeding process
  for(i in 1:pop_len){
    #extract two choromosomes for crossover
    chromo_list[[i]] <- chromo_mat[i, ]
    #separate the 2 choromosomes and decompose each one
    chromo_i1 <- unlist(strsplit(chromo_list[[i]][1], NULL))
    chromo_i2 <- unlist(strsplit(chromo_list[[i]][2], NULL))
    #extract the genes
    for(j in 1:gene_len){
      gene_array[1, j, i] <- chromo_i1[j]
      gene_array[2, j, i] <- chromo_i2[j]
    }
    #crossover
    cross_tmp <- crossover(gene_array[1, , i], gene_array[2, , i], cross_cutNum)
    
    for(j in 1:gene_len){
      #prepare for mutation
      chro_for_mut[1, j] <- unlist(cross_tmp[1])[j]
      chro_for_mut[2, j] <- unlist(cross_tmp[2])[j]
    }
    
    #mutation
    child_gene_mat1[i, ] <- mutation(as.numeric(chro_for_mut[1, ]), mutation_prob)
    child_gene_mat2[i, ] <- mutation(as.numeric(chro_for_mut[2, ]), mutation_prob)
  }
  
  #put two groups together, and the next generation is obtained
  nextGeneration <- rbind(child_gene_mat1, child_gene_mat2)

  
  return(nextGeneration)
}
