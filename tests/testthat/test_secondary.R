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
  expected <- c(-357.23560,-361.40398,-363.32859,-371.79320,-362.12230,22.50741)
  E <- evaluation(X,Y,Gen,6)
  expect_true(all.equal(E$fitness,expected,tolerance = 10^-3))
})

test_that('function breed behaves correctly',{

})
