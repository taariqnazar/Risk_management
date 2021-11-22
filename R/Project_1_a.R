#### Sampling ####


# Simulate a sample of suitable size n
# from the distribution of (Z_1, . . . , Z_{30})
sample_func <- function(years, sample_size) {
  return (replicate(sample_size, rnorm(years)))
}


rebalance <- function(k, p = 0.5, c = 0.5) {
  return(p*(1-c(k-1)/30))
}
years <- 30; sample_size <- 5;
samples <- sample_func(years, sample_size)
portfolio_value <- function(samples, mean, sigma, p, c) {
  # Create an empty matrix to store the values of V30
  store_V30 <- matrix(0,dim(samples)[2],1) # (value to fill, rows, columns)
  # Iterate through the sample_size, i.e. the column of samples
  for (sample_col in 1:dim(samples)[2]) { 
    # Calculate V30 using recursion, consider the boundary condition
    R_ZCP <- exp(0.01)
    R_stock <- exp(mean + sigma*samples[1,sample_col])
    V <- 1000 * rebalance(1,p,c) * R_stock + 1000 *(1 - rebalance(1,p,c)) * R_ZCP
    for (year in 2:30) {
      R_stock <- exp(mean + sigma*samples[year,sample_col])
      V <- rebalance(year,p,c) * (1000 + V) * R_stock + (1-rebalance(year,p,c)) * (1000 + V) * R_ZCP
    }
    store_V30[sample_col] <- V
  }
  return (store_V30)
}
mean <- 0.03; sigma <- 0.2; p <- 1e-5; c <- 5e-5
portfolio_value(samples, mean, sigma, p, c)

#### Empirical Distribution Function ####

# The empirical distribution function, the definition can be found in p. 201
emp_dist_func <- function(x, sample_vec) { # Note the sample_vec is equal to V30 that was derived above
  # Consider the case when x is an number 
  if (length(x) == 1) {
    emp_dist <- 0
    # The sample_vec is a vector containing the samples
    for (k in 1:length(sample_vec)) {
      if (sample_vec[k] < x) {
        emp_dist <- emp_dist + 1 # Note we have not divided by n yet
      }
    }
    return (emp_dist / length(sample_vec)) # Divide by n as per definition
  }
  # Consider the case when x is a sequence 
  emp_dist <- matrix(0,length(x),1)
  for (j in 1:length(x)) {
    temp <- 0
    for (k in 1:length(sample_vec)) {
      if (sample_vec[k] < x[j]) {
        temp <- temp + 1 # Note we have not divided by n yet
      }
    }
    emp_dist[j] <- temp/length(sample_vec)
  }
  return (emp_dist) 
}
years <- 30; sample_size <- 10000;
samples <- sample_func(years, sample_size)
x <- seq(35160.8,35161.8, length <-  sample_size)
V30 <- portfolio_value(samples, mean, sigma, p, c)
emp_cdf <- emp_dist_func(x,V30)

title <- paste("Empirical Distribution, with Sample Size <-", toString(sample_size))
plot(x, emp_cdf, col<-"red", xlab<-"Sequence", ylab<-"Probability",
     type<-"l",lwd<-1,  main<-title,cex.main<-0.8, cex.axis<-0.8)
legend("bottomright",legend<-c("Empirical Dist."), col<-c("red"), lwd<-1:2, cex<-0.6)







