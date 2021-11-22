# Step 1: create the guaranteed return function

# Guaranteed Return
G_func<- function(years, r_bar) {
  if (years == 0) {
    return (0)
  }
  sum <- 0
  for (year in 1:years) {
    sum <- sum + exp(0.005 * year)
  }
  G <- 1000*sum
  return (G)
}

# Step 2: Create sampling for V

# Simulate a sample of suitable size n
# from the distribution of (Z_1, . . . , Z_{30})
sample_func <- function(sample_size, years = 30) {
  return (replicate(sample_size, rnorm(years)))
}

# Step 3: Build the portfolio

portfolio_value <- function(samples, mean, sigma, p, c, r_bar = 0.005) {
  # Create an empty matrix to store the values of V30
  store_V30 <- matrix(0,dim(samples)[2],1) # (value to fill, rows, columns)
  r <- 0.01
  # R_ZCP <- Bank Account, i.e., risk free asset (zero coupon bond)
  R_ZCP <- exp(r)
  # Iterate through the sample_size, i.e. the column of samples
  for (sample_col in 1:dim(samples)[2]) { 
    # R_stock <- Risky Asset
    R_stock <- exp(mean + sigma*samples[1,sample_col])
    # Consider the base case
    V <- G_func(1, r_bar) * exp(-r) * R_ZCP+ (0+1000-G_func(1, r_bar)*exp(-r))*R_stock
    for (year in 2:30) {
      R_stock <- exp(mean + sigma*samples[year,sample_col])
      bank_account <- G_func(year, r_bar) * exp(-r) * R_ZCP
      risky_asset <- (V+1000-G_func(year, r_bar)*exp(-r))*R_stock
      
      V <-  bank_account + risky_asset
    }
    store_V30[sample_col] <- V
  }
  return (store_V30)
}
# Step 4: Plot a histogram of V30
mean <- 0.03; sigma <- 0.2; p <- 1e-5; c <- 5e-5
vec_V30 <- portfolio_value(sample_func(10000), mean, sigma, p, c)
hist(vec_V30)

# Step 5: Expected value E[V30]

# On p. 201 n^{-1} sum Z_k -> E[Z] when n -> infinity
mean <- 0.03; sigma <- 0.2; p <- 1e-5; c <- 5e-5
sample_size <- 10000 # sample_size <- n
vec_V30 <- portfolio_value(sample_func(sample_size), mean, sigma, p, c) # vec_V30 <- Z
mean <- sample_size^(-1) * sum(vec_V30)

# Step 6: 1% Empirical quantile function

# On p. 204 F^{-1}_n(p) = X_[n(1-p)+1],n 
emp_quantile_func <- function(obs, p=0.01) {
  # Declare array with X_1,n > X_2,n ...
  sorted_obs <- rev(sort(obs)) 
  n <- length(obs)
  # In video 7.2, [] is declared by the integer part or floor function
  index <- floor(n*(1-p))+1
  return(sorted_obs[index])
}
mean <- 0.03; sigma <- 0.2; p <- 1e-5; c <- 5e-5
vec_V30 <- portfolio_value(sample_func(10000), mean, sigma, p, c)
emp_quantile_func((vec_V30))
# Compare with
quantile(vec_V30,0.01)



# Last step : Results



























