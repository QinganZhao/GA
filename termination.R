# This is not a function, but an illustration of how the termination part would be inserted into select.R
# We can discuss about how we define convergence

# t: number of generations
# t_last: the last t_last generations for convergence check, default as 15
# t_max: the upperbound limit of generations

# two ways of determining convergence:
# running mean
approxEqual = function(a,b){
  abs(a - b) < .Machine$double.eps * abs(a + b)
}

# the convergence on alleles
# cvgRate_allele: the proportion of genes on one allele that are the same
# cvgRate_chrom: the proportion of alleles that are converged


