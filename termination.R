# This is not a function, but an illustration of how the termination part would be inserted into select.R
# We can discuss about how we define convergence

# t: number of generations
# t_last: the last t_last generations for convergence check, default as 15

approxEqual = function(a,b){
  abs(a - b) < .Machine$double.eps * abs(a + b)
}


select <- function(X, Y, popNum = 100, t_last = 15, .......){
  # setup
  
  # initialization
  
  # evaluation
  
  # termination
  t = 1
  min_record <- array(dim = 0)
  min_record <- c(min_record, min(df_current$fitness))
  
  while(t < t_last){
    # ranking
    # choosing
    # crossover
    # mutation
    
    t = t + 1
  }
  
  while(approxEqual(min(df_init$fitness), mean(min_record[(t - t_last + 1):t])) = FALSE){
    # ranking
    # choosing
    # crossover
    # mutation
    t = t + 1
    }
}


