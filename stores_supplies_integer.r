library("gramEvol")

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
    return(sum(ceiling(transport_cost/6)))
  }
  return(1e5)
}


monitorFunc <- function(result) {
  # print(result)
  cat("Best of gen: ", min(result$best$cost), "-", matrix(result$best$genome, nrow = 5, ncol = 3, byrow = TRUE), "\n")
}

# first try
x <- GeneticAlg.int(genomeLen = 15, codonMin = 0, codonMax = 250,
                    allowrepeat = TRUE, terminationCost = 5000,
                    monitorFunc = monitorFunc, evalFunc = evalFunc,
                    iterations = 100, popSize = 100)

# second try
# x <- GeneticAlg.int(genomeLen = 15, codonMin = 0, codonMax = 250,
#                     allowrepeat = TRUE, terminationCost = 5000,
#                     monitorFunc = NULL, evalFunc = evalFunc,
#                     iterations = 10000, popSize = 100)
print(x)

# exemplary solutions
# Best of gen:  5035 - 0 0 180 0 0 80 0 200 0 0 160 0 220 0 0 
# Best of gen:  5035 - 0 0 0 0 220 0 0 200 160 0 180 80 0 0 0
solution <- matrix(x$best$genome, nrow = 5, ncol = 3, byrow = TRUE)
transport_cost <- matrix(c(100, 60, 30,
                           80 , 50, 40,
                           60 , 40, 50,
                           50 , 30, 50,
                           40 , 60, 90), nrow = 5, ncol = 3, byrow = TRUE)*solution
sum(ceiling(transport_cost/6))
