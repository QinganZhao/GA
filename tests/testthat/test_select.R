context("Test the primary function: select()")

context("Test the input")

test_that('finds errors', {
  #dimension error
  expect_error(select(c(1,2),3),"Invalid data: X and Y do not match")

  #non numerical error
  expect_error(select("a","b"),)

  #number of iterations
  expect_error(select(1:3,1:3,min_iter = 5,max_iter = 2),"the maximum number of iteration
         must be bigger than the minimum number of iteration")

})

context("Test the warnings in the case of negative infinite AIC")

test_that('warnings',{
  #overfitting
  expect_warning(select(1,2,popNum = 2,min_iter = 1,max_iter = 2),
                 "There is an infinite AIC \\(might be overfitting\\)",all = FALSE)
})

context("Test the case that there are NAs in the input")

test_that('finds best predictors in spite of NAs', {
  X <- data.frame(matrix(ncol = 20, nrow = 20))
  X[,1] <- 1:20
  for (i in 2:20){X[,i]<- rep(1,20)}
  S <- select(X,1:20)
  names(S$coefficients) <- NULL
  correct <- rep(NA,length(S$coefficients))
  correct[1:2] <- c(0,1)
  expect_true(all.equal(S$coefficients,correct,tolerance = 10^-12))
})

context("Test if select() can find best predictors with default using simulated data")

test_that('finds best predictors with default', {
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
})

context("Test if select() can find best predictors without using rank using simulated data")

test_that('finds best predictors without using rank', {
  X <- data.frame(matrix(ncol = 20, nrow = 20))
  for (i in 1:20){X[,i]<- rep(i,20)+runif(20,0,1)}
  S <- select(X,1+2*X[,1]-3*X[,2]+0.0001*X[,3],usingRank = FALSE)
  names(S$coefficients) <- NULL
  correct <- rep(0,length(S$coefficients))
  correct[1:4] <- c(1,2,-3,0.0001)
  expect_true(all.equal(S$coefficients,correct,tolerance = 10^-12))
})

context("Test using select() with parallelization")

test_that('finds best predictors with parallelization', {
  X <- data.frame(matrix(ncol = 20, nrow = 20))
  for (i in 1:20){X[,i]<- rep(i,20)+runif(20,0,1)}
  S <- select(X,1+2*X[,1]-3*X[,2]+0.0001*X[,3],useParallel = TRUE)
  names(S$coefficients) <- NULL
  correct <- rep(0,length(S$coefficients))
  correct[1:4] <- c(1,2,-3,0.0001)
  expect_true(all.equal(S$coefficients,correct,tolerance = 10^-12))
})

context("Test if select() can find best predictors based on glm using simulated data")

test_that('finds best predictors with glm', {
  X <- data.frame(matrix(ncol = 20, nrow = 20))
  for (i in 1:20){X[,i]<- rep(i,20)+runif(20,0,1)}
  S <- select(X,1+2*X[,1]-3*X[,2]+0.0001*X[,3],reg = 'glm')
  names(S$coefficients) <- NULL
  correct <- rep(0,length(S$coefficients))
  correct[1:4] <- c(1,2,-3,0.0001)
  expect_true(all.equal(S$coefficients,correct,tolerance = 10^-12))
})

context('REAL DATA: Test if most strong predictors in a real-world dataset can be selected')

test_that('test if most strong predictors in a real-world dataset can be selected', {
  download.file('https://github.com/QinganZhao/Data-Science/blob/master/database/JGSSdata?raw=true', 'jp')
  load('jp')
  jpY <- jp[,c("SZINCOMA")]
  Yjp <- as.matrix(log(jpY))
  Xjp <- jp[,c("SEXA", "AGEB","MARC", "XJOB1WK", "XXLSTSCH", "JOINUNI","PPLVTG","DAY",
               "FQSPORT","DOSMOKEX","BD3SAFTY","Q4NOPWR","OPCO2EM","APPCCSXB","Q4NOCCMG",
               "OPTEED", "ST5HLTHY", "DAYB", "MONTH", "DORL", "Q4DIVOK", "OP4NAME", "WLACCORG", 
               "ST5LIFEY", "TP5LOC15", "PPLSTSCH")]
  Xjp <- as.matrix(Xjp)
  StrongPredictors <- c("predictor.SEXA", "predictor.AGEB", "predictor.MARC", 
                        "predictor.XJOBIWK", "predictor.XXLSTSCH", "predictor.ST5LIFEY",
                        "predictor.TP5LOC15", "predictor.PPLSTSCH")
  lmSelect <- select(Xjp, Yjp, reg = 'lm')
  glmSelect <- select(Xjp, Yjp, reg = 'glm')
  lmResult <- names(lmSelect$coefficients)
  glmResult <- names(glmSelect$coefficients)
  
  #check if at least 5/8 of the known strong predictors have been selected
  checkLm <- sum(StrongPredictors %in% lmResult) >= 5
  checkGlm <- sum(StrongPredictors %in% glmResult) >= 5
  expect_equal(checkLm, TRUE)
  expect_equal(checkGlm, TRUE)
})

