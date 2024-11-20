# Three numeric predictors stored in matrix X

 set.seed(123)
 n = 100
 p = 3
 X = matrix(rnorm(n * p), nrow = n, ncol = p)
 time = rexp(n, rate = 1)
 status = sample(0:1, n, replace = TRUE)
 df = as.data.frame(X)
 df$time <- time
 df$status <- status
