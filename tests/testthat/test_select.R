

test_that('finds errors', {
  expect_error(select(c(1,2),3),"")
})

test_that('finds best predictors', {
  #values with NA
  X <- data.frame(matrix(ncol = 20, nrow = 20))
  X[,1] <- 1:20
  for (i in 2:20){X[,i]<- rep(1,20)}
  S <- select(X,1:20)
  names(S$coefficients) <- NULL
  correct <- rep(NA,length(S$coefficients))
  correct[1:2] <- c(0,1)
  expect_true(all.equal(S$coefficients,correct,tolerance = 10^-12))

  #obvious dataframe
  X <- data.frame(matrix(ncol = 20, nrow = 20))
  X[,1] <- 1:20
  for (i in 2:20){X[,i]<- rep(i,20)+runif(20,0,1)}
  S <- select(X,1:20,usingRank = FALSE)
  names(S$coefficients) <- NULL
  correct <- rep(0,length(S$coefficients))
  correct[2] <- 1
  expect_true(all.equal(S$coefficients,correct,tolerance = 10^-12))

  #less obvious regression
  X <- data.frame(matrix(ncol = 20, nrow = 20))
  for (i in 1:20){X[,i]<- rep(i,20)+runif(20,0,1)}
  S <- select(X,1+2*X[,1]-3*X[,2]+0.0001*X[,3])
  names(S$coefficients) <- NULL
  correct <- rep(0,length(S$coefficients))
  correct[1:4] <- c(1,2,-3,0.0001)
  expect_true(all.equal(S$coefficients,correct,tolerance = 10^-12))

  #without using rank
  S <- select(X,1+2*X[,1]-3*X[,2]+0.0001*X[,3],usingRank = FALSE)
  names(S$coefficients) <- NULL
  correct <- rep(0,length(S$coefficients))
  correct[1:4] <- c(1,2,-3,0.0001)
  expect_true(all.equal(S$coefficients,correct,tolerance = 10^-12))

  #using parallelization
  S <- select(X,1+2*X[,1]-3*X[,2]+0.0001*X[,3],useParallel = TRUE)
  names(S$coefficients) <- NULL
  correct <- rep(0,length(S$coefficients))
  correct[1:4] <- c(1,2,-3,0.0001)
  expect_true(all.equal(S$coefficients,correct,tolerance = 10^-12))

  #using glm
  S <- select(X,1+2*X[,1]-3*X[,2]+0.0001*X[,3],reg = 'glm')
  names(S$coefficients) <- NULL
  correct <- rep(0,length(S$coefficients))
  correct[1:4] <- c(1,2,-3,0.0001)
  expect_true(all.equal(S$coefficients,correct,tolerance = 10^-12))
})


