library(Rcpp)
library(e1071)
library(microbenchmark)
library(parallel)

##### Problem 1. #####
cppFunction('
double C_moment(NumericVector v, int k) {
  int n = v.size();
  if (n == 0) return NA_REAL;
  
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    sum += v[i];
  }
  double mean = sum / n;
  
  double acc = 0.0;
  for (int i = 0; i < n; ++i) {
    double diff = v[i] - mean;
    double term = 1.0;
    for (int j = 0; j < k; ++j) {
      term *= diff;
    }
    
    acc += term;
  }
  
  return acc / n;
}')

set.seed(123)
x <- rnorm(100)
k <- 3

C_moment(x, k)
# 0.04532669
moment(x, order = k, center = TRUE, absolute = FALSE)
# 0.04532669

microbenchmark(
  C_moment(x, k),
  moment(x, order = k, center = TRUE, absolute = FALSE),
  times = 50
)

##### Problem 2. #####
source("ProblemSet05.R")

#Definition
setClass(
  Class = "bootstrapWaldCI",
  contains = "waldCI",
  slots = list(
    fun = "function",
    data = "ANY",
    reps = "integer",
    compute = "character",
    bootStats = "numeric"
  )
)

# Validity function
setValidity("bootstrapWaldCI", function(object) {
  msgs <- character()
  if (length(object@reps) != 1 || object@reps <= 0) {
    msgs <- c(msgs, "reps must be a single positive integer")
  }
  if (!object@compute %in% c("serial", "parallel")) {
    msgs <- c(msgs, "compute must be either 'serial' or 'parallel'")
  }
  if (length(object@bootStats) != as.integer(object@reps)) {
    msgs <- c(msgs, "length(bootStats) must equal number of reps")
  }
  if (length(msgs) == 0) TRUE else msgs
})

#Helper function
.runBootstrap <- function(FUN, data, reps, compute = c("serial", "parallel")) {
  compute <- match.arg(compute)
  FUN     <- match.fun(FUN)
  reps    <- as.integer(reps)
  n <- nrow(data)
  
  one_boot <- function() {
    idx <- sample.int(n, n, replace = TRUE)
    
    if (is.null(dim(data))) {
      FUN(data[idx])
    } else {
      FUN(data[idx, , drop = FALSE])
    }
  }
  
  if (compute == "serial") {
    stats <- sapply(seq_len(reps), one_boot)
  } else {
    num_cores <- max(1, detectCores() %/% 2)
    cl <- makeCluster(num_cores)
    on.exit(stopCluster(cl), add = TRUE)
    stats <- parSapply(cl, seq_len(reps), one_boot)
  }
  
  as.numeric(stats)
}

#Constructor
makeBootstrapCI <- function(FUN,
                            data,
                            reps = 100,
                            level = 0.95,
                            compute = c("serial", "parallel")) {
  compute <- match.arg(compute)
  FUN <- match.fun(FUN)
  reps <- as.integer(reps)
  
  bootStats <- .runBootstrap(FUN, data, reps, compute)
  boot_mean <- mean(bootStats)
  boot_se <- sd(bootStats)
}