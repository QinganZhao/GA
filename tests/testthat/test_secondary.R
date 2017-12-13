test_that('function evaluation gives correct result',{
  #very simple data
  X <- matrix(c(1,2,3,4),ncol = 2)
  Y <- c(5,6)
  gen <- matrix(c(0,1,0,1,0,0,1,1),ncol = 2)
  E <- evaluation(X,Y,gen,4)

  expect_equal(E$fitness,rep(-Inf,4))

  #more complex data
  X <- data.frame(matrix(ncol = 6, nrow = 6))
  for (i in 1:6){
    for (j in 1:6){X[i,j]<- rep(3*i^2-i*j+7/j)}
  }
  Y <- 1+2*X[,1]-3*X[,2]+0.0001*X[,3]
  Gen <- upper.tri(matrix(1,6,6),diag = TRUE)
  expected <- c(-357.23560,-361.40398,-363.32859,
                -371.79320,-362.12230,22.50741)
  E <- evaluation(X,Y,Gen,6)
  expect_true(all.equal(E$fitness,expected,tolerance = 10^-3))
})

test_that('function superEvaluation gives correct result',{
  library(parallel)
  library(doParallel)
  library(foreach)
  #very simple data
  X <- matrix(c(1,2,3,4),ncol = 2)
  Y <- c(5,6)
  gen <- matrix(c(0,1,0,1,1,0,1,1),ncol = 2)
  E <- superEvaluation(X,Y,gen,4)
  rownames(E$fitness) <- NULL

  expect_equal(as.vector(E$fitness),rep(-Inf,4))

  #more complex data
  X <- data.frame(matrix(ncol = 6, nrow = 6))
  for (i in 1:6){
    for (j in 1:6){X[i,j]<- rep(3*i^2-i*j+7/j)}
  }
  Y <- 1+2*X[,1]-3*X[,2]+0.0001*X[,3]
  Gen <- upper.tri(matrix(1,6,6),diag = TRUE)
  expected <- c(-357.23560,-361.40398,-363.32859,
                -371.79320,-362.12230,22.50741)
  E <- superEvaluation(X,Y,Gen,6)
  rownames(E$fitness) <- NULL

  expect_true(all.equal(as.vector(E$fitness),expected,tolerance = 10^-3))
})

test_that('function crossover gives correct results',{
  #simple test for a non-random crossing
  expect_equal(crossover(c(0,0),c(1,1)),list(c(0,1),c(1,0)))

  #whatever the length of the chromosome should not change
  a <- floor(2*runif(10,0,1))
  b <- floor(2*runif(10,0,1))
  expect_equal(length(crossover(a,b,nCuts = 5)[[1]]),10)
})

test_that('functions breed and popInitial behave correctly',{
  #two identical generations should be unchanged if no mutation
  Gen <- 1*upper.tri(matrix(1,3,3),diag = TRUE)
  df <- as.data.frame(matrix(nrow = 3, ncol = 2))
  df[1] <- apply(Gen, 1, paste, collapse = '')
  df[2] <- apply(Gen, 1, paste, collapse = '')
  M <- matrix(c(1,0,0,1,0,0,1,1,0,1,1,0,1,1,1,1,1,1),ncol = 3)
  expect_equal(breed(df,mutation_prob = 0),M)

  #without mutation, the number of 1-alleles must be constant
  ch1 <- popInitial(10,10,0.5)
  ch2 <- popInitial(10,10,0.2)
  df <- as.data.frame(matrix(nrow = 10, ncol = 2))
  df[1] <- apply(ch1, 1, paste, collapse = '')
  df[2] <- apply(ch2, 1, paste, collapse = '')
  expect_equal(sum(breed(df,mutation_prob = 0)),130)
})

test_that('function rankSelection behaves correctly',{
  df <- data.frame(matrix(runif(10,0,10),ncol = 2))
  colnames(df)<- c('chromosome','fitness')
  expect_equal(rankSelection(df)$probability,c(1/3,4/15,1/5,2/15,1/15))
})
