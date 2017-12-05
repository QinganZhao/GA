# this function takes for input 2 genes and creates 2 new genes from them
# the last input is the  umber of cuts in allowed in the genes (default is 1)

crossover <- function(ch1,ch2,n_cuts = 1) {
  #length of the chromosome
  L <- length(ch1)
  
  if (L != length(ch2)) {return("the two chromosomes must have the same length")}
  if (!is.integer(n_cuts)) {return("the number of cuts must be an integer")}
  
  #assign indexes to the cuts
  #for more crossing possibilities, we only keep unique indexes for cuts
  #(so the actual number of cuts may be smaller than n_cuts, if 2 cuts are made
  #at the same spot)
  cuts <- unique(floor(runif(n_cuts,1,L)))
  #reorder cut indexes, and add L as the final cut
  cuts <- c(cuts[order(cuts)],L)
  n_cuts <- length(cuts)-1
  
  c = 1 #indexes the cuts
  i = 1 #indexes the genes
  
  #initialization: ch_son are the returned chromosomes
  ch_son1 <- ch1[1:cuts[1]]
  ch_son2 <- ch2[1:cuts[1]]
  #a and b are temporaty copies that can be switched
  a <- ch2
  b <- ch1
  
  #loop over the number of cuts
  while (c < n_cuts+1) {
    #assign the portion between 2 cuts to son-chromosomes
    i <- cuts[c]+1
    ch_son1 <- c(ch_son1,a[i:cuts[c+1]])
    ch_son2 <- c(ch_son2,b[i:cuts[c+1]])

    #switch a and b
    tmp <- a
    a <- b
    b <- tmp
    
    #increment
    c <- c +1
  }
  
  return(list(ch_son1,ch_son2))
}
  