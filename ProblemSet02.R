##### Problem 1 #####

### a. ###
random_walk1 <- function(n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  position <- 0
  for (i in 1:n) {
    step <- runif(1) #Pulls random value from uniform dist.
    if (step < 0.5) {
      if (runif(1) < 0.05) {
        position <- position + 10
      } else {
        position <- position + 1
      }
    } else {
      if (runif(1) < 0.2) {
        position <- position - 3
      } else {
        position <- position - 1
      }
    }
  }
  
  return(position)
}

random_walk2 <- function(n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  u <- runif(2 * n)
  dir <- u[seq(1, 2*n, by = 2)] < 0.5    #Direction
  sub <- u[seq(2, 2*n, by = 2)]          #Subtype
  steps <- ifelse(dir,
                  ifelse(sub < 0.05, 10, 1),   
                  ifelse(sub < 0.2, -3, -1))
  
  sum(steps)
}

random_walk3 <- function(n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  steps <- sapply(1:n, function(i) {
    if (runif(1) < 0.5) {
      if (runif(1) < 0.05) 10 else 1
    } else {
      if (runif(1) < 0.2) -3 else -1
    }
  })
  
  sum(steps)
}

random_walk1(10)
random_walk2(10)
random_walk3(10)
random_walk1(1000)
random_walk2(1000)
random_walk3(1000)

### b. ###
random_walk1(10, seed=42)
random_walk2(10, seed=42)
random_walk3(10, seed=42)

random_walk1(1000, seed=42)
random_walk2(1000, seed=42)
random_walk3(1000, seed=42)


### c. ###
library(microbenchmark)

#For low input
microbenchmark(
  loop = random_walk1(1000),
  vectorized = random_walk2(1000),
  apply = random_walk3(1000),
  times = 100
)

#For high input
microbenchmark(
  loop = random_walk1(100000),
  vectorized = random_walk2(100000),
  apply = random_walk3(100000),
  times = 10
)
