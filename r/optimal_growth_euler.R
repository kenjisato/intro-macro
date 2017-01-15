library(ggplot2)
library(ggthemes)
library(tibble)

# Parameters ----

alpha = 0.3
theta = 5
delta = 0.05
rho = 0.1

# Functions ----

f = function(k){
  return(k^alpha)
}

u = function(c){
  return((c^(1 - theta) - 1) / (1 - theta)) 
}

# dk/dt = 0 locus

c_steady = function(k){
  return(f(k) - delta * k)
}

k_star = inv_df(delta + rho)


# simulation code ----

update_k = function(k, c, dt=0.01){
  return(k + dt * (f(k) - delta * k - c))
}

update_c = function(k, c, dt=0.01){
  return(c * (1 + dt * (df(k) - delta - rho) / theta))
}

search_policy = function(k0, min_c0, max_c0, max_step, dt=0.01, max_iter=1e4){
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Find the optimal consumption policy for a given initial capital stock k0
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #
  # Parameters
  # """"""""""
  # k0: numeric
  #     initial capital stock
  #
  # min_c0: numeric
  # max_c0: numeric
  #     The interval [cmin, cmax] is the range of guess which the analyst 
  #     (you) believes that the optimal c0 lies between.
  # 
  # max_step: integer
  #     The max step count for which monotonicity is verified.
  #     If a simulation maintains monotonicity for sufficiently long,
  #     it is understood that this path is very close to optimal.
  # 
  # dt: numeric
  #     Step size of simulation passed to `update_c()` and `update_k()`.
  #
  # max_iter: integer
  #     Iteration limit.
  #
  # Returns
  # """""""
  #
  # optimal_path: tibble
  #     optimal path that starts from (k0, c0)
  # 
  # Example
  # """""""
  # 
  # To be given...
  # 
  #
  
  counter = 0
  path = as.matrix(tibble(k=numeric(max_step), c=numeric(max_step)))
  
  while (TRUE && counter < max_iter) {
    counter = counter + 1
    
    c0 = (min_c0 + max_c0) / 2
    
    path[1, 'k'] = k0
    path[1, 'c'] = c0
    
    for (i in seq_len(max_step - 1)) {
      path[i + 1, 'k'] = update_k(path[i, 'k'], path[i, 'c'], dt)
      path[i + 1, 'c'] = update_c(path[i, 'k'], path[i, 'c'], dt)
    }
    valid_nrow = min(sum(!is.na(path[,'k'])), sum(!is.na(path[,'c'])))
    valid_path = path[1:valid_nrow, ] 
    
    k_increasing = all(valid_path[2:valid_nrow, 'k'] - 
                         valid_path[1:valid_nrow - 1, 'k'] >= 0)
    k_decreasing = all(valid_path[2:valid_nrow, 'k'] - 
                         valid_path[1:valid_nrow - 1, 'k'] <= 0)
    c_increasing = all(valid_path[2:valid_nrow, 'c'] - 
                         valid_path[1:valid_nrow - 1, 'c'] >= 0)
    c_decreasing = all(valid_path[2:valid_nrow, 'c'] - 
                         valid_path[1:valid_nrow - 1, 'c'] <= 0)
    
    increasing = k_increasing && c_increasing
    decreasing = k_decreasing && c_increasing
    
    if (increasing || decreasing) {
      break
    } else if (k_increasing && !c_increasing) {
      # Too small c0
      min_c0 = c0
    } else if (k_decreasing && !c_decreasing) {
      # Too large c0
      max_c0 = c0
    } else if (k_increasing && c_decreasing) {
      # Too small c0
      min_c0 = c0
    } else if (k_decreasing && c_increasing) {
      # Too large c0
      max_c0 = c0
    } else if (c_increasing && !k_increasing) {
      # Too large c0
      max_c0 = c0
    } else if (c_decreasing && !k_decreasing) {
      # Too small c0
      min_c0 = c0
    }
  }
  
  optimal_path = as_tibble(valid_path)
  return(optimal_path)
}


# Plotting ----

kmin = 0.0; kmax = 30.0
cmin = 0.0; cmax = 3.0

kgrid = seq(kmin, kmax, by=0.01)

p = qplot(kgrid, c_steady(kgrid), geom='line') + 
  geom_vline(xintercept=k_star) + theme_gdocs() +
  labs(x='k', y='c') + 
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limits=c(cmin, cmax))

optimum = search_policy(k0=6, min_c0=1.619, max_c0=1.621, 
                        max_step=7000, max_iter=2000)
(p_optimum = p + geom_path(data=optimum, aes(x=k, y=c), color="blue"))
