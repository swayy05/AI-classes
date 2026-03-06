library("genalg")

evalFunc <- function(X=c()) {
  solution <- matrix(X, nrow = 5, ncol = 3, byrow = TRUE)
  
  # sum over columns (<=)
  cond_columns <- (colSums(solution) <= c(750, 640, 450))
  
  # sum over row (>=)
  cond_rows <- (rowSums(solution) >= c(180, 80, 200, 160, 220))
  
  if (sum(!cond_columns) + sum(!cond_rows) == 0) {
    transport_cost <- matrix(c(100, 60, 30,
                               80 , 50, 40,
                               60 , 40, 50,
                               50 , 30, 50,
                               40 , 60, 90), nrow = 5, ncol = 3, byrow = TRUE)*solution
    return(sum(transport_cost))
  }
  return(1e8)
}

monitorFunc <- function(result) {
  cat("Best of gen: ", min(result$evaluations), "\n")
}

lower_bounds <- rep(0, times=15)
upper_bounds <- rep(250, times=15)
# first try
rbga.results = rbga(stringMin=lower_bounds, stringMax=upper_bounds, popSize=200, 
                    evalFunc=evalFunc, monitorFunc=monitorFunc,
                    iters=1000, verbose=FALSE, mutationChance=0.01)

# second try
rbga.results = rbga(stringMin=lower_bounds, stringMax=upper_bounds, popSize=50,
                    evalFunc=evalFunc, monitorFunc=NULL,
                    iters=10000, verbose=FALSE, mutationChance=0.01)

# summary of the computations
summary(rbga.results, echo=TRUE)

# get result from the population
filter <- rbga.results$evaluations == min(rbga.results$evaluations)
bestObjectCount <- sum(rep(1, rbga.results$popSize)[filter])
if (bestObjectCount > 1) {
  bestSolution = rbga.results$population[filter,][1,]
} else {
  bestSolution = rbga.results$population[filter,]
}

solution <- matrix(bestSolution, nrow = 5, ncol = 3, byrow = TRUE)
transport_cost <- matrix(c(100, 60, 30,
                           80 , 50, 40,
                           60 , 40, 50,
                           50 , 30, 50,
                           40 , 60, 90), nrow = 5, ncol = 3, byrow = TRUE)*solution
sum(transport_cost)


# Excel solution
solution <- matrix(c(0,	0,	180,
                     0,	0,	80,
                     0,	200,	0,
                     0,	160,	0,
                     220,	0,	0), nrow = 5, ncol = 3, byrow = TRUE)
transport_cost <- matrix(c(100, 60, 30,
                           80 , 50, 40,
                           60 , 40, 50,
                           50 , 30, 50,
                           40 , 60, 90), nrow = 5, ncol = 3, byrow = TRUE)*solution
sum(transport_cost)
# best solution: 30200

# plot the progress
plot(rbga.results$best)
