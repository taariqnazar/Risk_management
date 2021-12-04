library(copula)
gaussian <- function(simu) {
  rho <- (pi/2) * sin(0.4)
  dim <- 50
  norm.cop <- normalCopula(rho, dim = dim, dispstr = "ex")
  paramMargins <- list()
  for (i in 1:50){
    paramMargins[[i]] <- list(3)
  }
  margins <- c(rep("t",50))
  md <-mvdc(norm.cop, margins, paramMargins)
  return(rMvdc(simu,md))
}
t <- function(simu) {
  t.cop <- tCopula(rho, dim = dim, dispstr = "ex", df = 4)
  paramMargins <- list()
  
  for (i in 1:50){
    paramMargins[[i]] <- list(3)
  }
  margins <- c(rep("t",50))
  md <-mvdc(t.cop, margins, paramMargins)
  
  sim <- 100
  return(rMvdc(simu,md))
}
clayton <- function(simu) {
  clayton.copula <- archmCopula(family = "clayton", dim = 3, param = 2)
  paramMargins <- list()
  
  for (i in 1:50){
    paramMargins[[i]] <- list(3)
  }
  margins <- c(rep("t",50))
  md <-mvdc(t.cop, margins, paramMargins)
  
  sim <- 1000
  return(rMvdc(simu,md))
}
V_1_func <- function(sample, sim) {
  # Store V_1 for each simulation
  V_1<- 2*10^4 * sum(exp(sample))
  return(V_1)
}
V_0_func <- function() {
  return(50*2*10^4)
}
var_func <- function(payoff) {
  R_0 <- 1
  L <- -1*payoff/R_0
  L <- sort(L, decreasing = TRUE)
  p <- 0.01
  index <- floor(length(L) * p) + 1
  return(L[index])
}
es_func <- function(payoff){
  R_0 <- 1
  L <- -1*payoff/R_0
  L <- sort(L, decreasing = TRUE)
  n <- length(L)
  p <- 0.01
  if (floor(n * p) == 0) stop("floor(n * p) == 0 is True ")
  index <- floor(n * p)
  es <- matrix(0,index)
  for (k in 1:index) {
    es[k] = (1/p)*(L[k]/n + ( p - index/n)*L[index + 1])
  }
  return(sum(es))
}
comp <- function(samples, sim) {
  # Calculate the net payoff
  payoff <- matrix(0, sim)
  for (i in 1:sim) {
    sample <- samples[i,]
    payoff[i] <- V_1_func(sample) - V_0_func()
  }
  # Calculate VaR and ES
  var <- var_func(payoff)
  es <- es_func(payoff)
  res <- c(var, es)
  return(res)
}

######### Gaussian distribution #########
# small test 
sim <- 1000
sample <- gaussian(sim)
res <- comp(sample, sim)
table <- matrix(0,1,3)
colnames(table) <- c("sim", "VaR", "ES")
table[1] <- sim; table[2] <- res[1]; table[3] <- res[2]
print(table)

# The interval of the simulation
sim_arr <- seq(10^2,10^4,10^2) 
len_sim_arr <- length(sim_arr)
var <- matrix(0,len_sim_arr); es <- matrix(0,len_sim_arr);
table <- matrix(0,len_sim_arr,3); colnames(table) <- c("sim", "VaR", "ES")
for (i in 1:length(sim_arr)) {
  sample <- gaussian(sim_arr[i])
  res <- comp(sample, sim_arr[i])
  var[i] <- res[1]
  es[i] <- res[2]
  table[i,1] <- sim_arr[i]; table[i,2] <- res[1]; table[i,3] <- res[2]
}
print(table)

par(mfrow=c(2,1))
plot(var)
plot(es)

  
  
  
  
  
  
  
  
  
  
  





