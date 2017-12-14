# this function takes for input 2 genes and creates 2 new genes from them
# the last input is the  umber of cuts in allowed in the genes (default is 1)



#' support function: crossover()
#'
#' This function takes for input 2 genes and creates 2 new genes from them. 
#' @param ch1 string; the first parent
#' @param ch2 string; the second parent
#' @param nCuts integer; How many cuts do you want for the crossover? Defaults to 1
#' @keywords crossover
#' @export
#' @examples
#' crossover()
crossover <- function(ch1,ch2,nCuts = 1) {
  #length of the chromosome
  L <- length(ch1)
  
  if (L != length(ch2)) {stop("the two chromosomes must have the same length")}
  if (round(nCuts) != nCuts) {stop("the number of cuts must be an integer")}
  
  #assign indexes to the cuts
  #for more crossing possibilities, we only keep unique indexes for cuts
  #(so the actual number of cuts may be smaller than nCuts, if 2 cuts are made
  #at the same spot)
  cuts <- unique(floor(runif(nCuts,1,L)))
  #reorder cut indexes, and add L as the final cut
  cuts <- c(cuts[order(cuts)],L)
  nCuts <- length(cuts)-1
  
  c = 1 #indexes the cuts
  i = 1 #indexes the genes
  
  #initialization: chSon are the returned chromosomes
  chSon1 <- ch1[1:cuts[1]]
  chSon2 <- ch2[1:cuts[1]]
  #a and b are temporaty copies that can be switched
  a <- ch2
  b <- ch1
  
  #loop over the number of cuts
  while (c < nCuts+1) {
    #assign the portion between 2 cuts to son-chromosomes
    i <- cuts[c]+1
    chSon1 <- c(chSon1,a[i:cuts[c+1]])
    chSon2 <- c(chSon2,b[i:cuts[c+1]])

    #switch a and b
    tmp <- a
    a <- b
    b <- tmp
    
    #increment
    c <- c +1
  }
  
  return(list(chSon1,chSon2))
}
  
