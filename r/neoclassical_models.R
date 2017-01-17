# Purpose ----
# 
# The purpose of this script is to build higher-order function
# that generate frequently used functions in macroeconomics.
# 


# Production Functions ----

cobbdouglas = function(A, alpha) {
  F = function(K, L) {
    return(A * K ^ alpha * L)
  }
  return(F)
}

cobbdouglas_intensive = function(A, alpha) {
  f = function(k) return(A * k^alpha)
  return(f)
}

crra_utility = function(theta) {
  if (theta == 1){
    return(log)
  }
  u = function(c){
    return((c^(1 - theta) - 1) / (1 - theta)) 
  }
  return(u)
}


# dk/dt = 0 locus

c_steady = function(k){
  return(f(k) - delta * k)
}

k_star = inv_df(delta + rho)
