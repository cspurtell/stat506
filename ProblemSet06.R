library(Rcpp)
library(e1071)
library(microbenchmark)
library(parallel)
library(dplyr)
library(lme4)
library(purrr)
library(ggplot2)

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
  
  one_boot <- function(i) {
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
  
  parent_ci <- makeWaldCI(
    level = level,
    mean = boot_mean,
    sterr = boot_se
  )
  
  obj <- new(
    "bootstrapWaldCI",
    lb = lb(parent_ci),
    ub = ub(parent_ci),
    level = level(parent_ci),
    fun = FUN,
    data = data,
    reps = reps,
    compute = compute,
    bootStats = bootStats
  )
  
  validObject(obj)
  obj
}

setGeneric("rebootstrap", function(object) standardGeneric("rebootstrap"))
setMethod("rebootstrap", "bootstrapWaldCI", function(object) {
  FUN <- object@fun
  data <- object@data
  reps    <- object@reps
  level   <- object@level
  compute <- object@compute
  
  bootStats <- .runBootstrap(FUN, data, reps, compute)
  boot_mean <- mean(bootStats)
  boot_se   <- sd(bootStats)
  z         <- qnorm((1 + level) / 2)
  half_w    <- z * boot_se
  
  object@lb        <- boot_mean - half_w
  object@ub        <- boot_mean + half_w
  object@bootStats <- bootStats
  
  validObject(object)
  object
})

### b. ###
ci1 <- makeBootstrapCI(function(x) mean(x$y),
                       ggplot2::diamonds,
                       reps = 1000)
ci1
rebootstrap(ci1)

#Time comparison
t_serial <- system.time({
  ci_serial <- makeBootstrapCI(function(x) mean(x$y), 
                               ggplot2::diamonds, 
                               reps = 3000, 
                               compute = "serial")
})
t_serial

t_parallel <- system.time({
  ci_parallel <- makeBootstrapCI(function(x) mean(x$y), 
                                 ggplot2::diamonds, 
                                 reps = 3000, 
                                 compute = "parallel")
})
t_parallel

### c. ###
dispCoef <- function(data) {
  fit <- lm(mpg ~ cyl + disp + wt, data = data)
  unname(coef(fit)["disp"])
}

ci2 <- makeBootstrapCI(dispCoef,
                       mtcars,
                       reps = 1000)
ci2
rebootstrap(ci2)

t_serial <- system.time({
  ci_serial <- makeBootstrapCI(dispCoef,
                         mtcars,
                         reps = 3000,
                         compute = "serial")
})
t_serial

t_parallel <- system.time({
  ci_parallel <- makeBootstrapCI(dispCoef,
                               mtcars,
                               reps = 3000,
                               compute = "parallel")
})
t_parallel

##### Problem 3. #####
source("ps06q3.R")

### a. ###
df_std <- df %>%
  group_by(country) %>%
  mutate(
    prior_gpa_z = scale(prior_gpa)[,1],
    forum_posts_z = scale(forum_posts)[,1],
    quiz_attempts_z = scale(quiz_attempts)[,1],
  ) %>% 
  ungroup()

df_by_country <- split(df_std, df_std$country)
fit_country_model <- function(data) {
  glmer(
    completed_course ~ prior_gpa_z + forum_posts_z + quiz_attempts_z + (1 | device_type),
    data = data,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa")
  )
}
models <- map(df_by_country, fit_country_model)

coef_df <- map_df(
  names(models),
  ~ {
    m <- models[[.x]]
    tibble(
      country = .x,
      est = fixef(m)["forum_posts_z"],
      se = sqrt(diag(vcov(m)))["forum_posts_z"]
    )
  }
)



#Resources used: R documentation for unname() function and bootstrapping assistance, ChatGPT for debugging