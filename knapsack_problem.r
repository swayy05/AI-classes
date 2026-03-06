# https://www.r-bloggers.com/2012/08/genetic-algorithms-a-simple-r-example/

library("genalg")
library("ggplot2")
library("animation")

dataset <- data.frame(item = c("pocketknife", "beans", "potatoes", "unions", 
                               "sleeping bag", "rope", "compass"), 
                      survivalpoints = c(10, 20, 15, 2, 30, 10, 30), 
                      weight = c(1, 5, 10, 1, 7, 5, 1))
weightlimit <- 20

# exemplary chromosome
chromosome = c(1, 0, 0, 1, 1, 0, 0)
dataset[chromosome == 1, ]
##           item survivalpoints weight
## 1  pocketknife             10      1
## 4       unions              2      1
## 5 sleeping bag             30      7
cat(chromosome %*% dataset$survivalpoints)
## 42


# optimisation
evalFunc <- function(x) {
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  
  if (current_solution_weight > weightlimit) {
    return(0)
  } else {
    return(-current_solution_survivalpoints)
  }
}

iter <- 100
GAmodel <- rbga.bin(size = 7, popSize = 200, iters = iter, mutationChance = 0.01, 
                    elitism = T, evalFunc = evalFunc)

# solution
summary(GAmodel, echo=TRUE)
## GA Settings
##   Type                  = binary chromosome
##   Population size       = 200
##   Number of Generations = 100
##   Elitism               = TRUE
##   Mutation Chance       = 0.01
## 
## Search Domain
##   Var 1 = [,]
##   Var 0 = [,]
## 
## GA Results
##   Best Solution : 1 1 0 1 1 1 1 

solution = c(1, 1, 0, 1, 1, 1, 1)
dataset[solution == 1, ]
##           item survivalpoints weight
## 1  pocketknife             10      1
## 2        beans             20      5
## 3     potatoes             15     10
## 4       unions              2      1
## 5 sleeping bag             30      7
## 7      compass             30      1

# solution vs available
cat(paste(solution %*% dataset$survivalpoints, "/", sum(dataset$survivalpoints)))
## 107 / 117

# visualisation
animate_plot <- function() {
  for (i in seq(2, iter)) {
    temp <- data.frame(Generation = c(seq(1, i), seq(1, i)), 
                       Variable = c(rep("mean", i), rep("best", i)), 
                       Survivalpoints = c(-GAmodel$mean[1:i], -GAmodel$best[1:i]))

    pl <- ggplot(temp, aes(x = Generation, y = Survivalpoints, group = Variable, colour = Variable)) + geom_line() + 
          scale_x_continuous(limits = c(0, iter)) + scale_y_continuous(limits = c(0, 110)) +
          geom_hline(yintercept = max(temp$Survivalpoints)) + scale_colour_brewer(palette = "Set1") +
          ggtitle(label = "Evolution Knapsack optimization model")
    
    print(pl)
  }
}

# save the animation
saveGIF(animate_plot(), interval = 0.1, outdir = getwd(), ani.height = 600, ani.width = 600)