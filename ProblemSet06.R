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

coef_df %>%
  mutate(
    lower = est - 1.96 * se,
    upper = est + 1.96 * se
  ) %>%
  ggplot(aes(x = est, y = country)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Estimated Effect of Forum Posts on Course Completion",
    x = "Coefficient (log-odds scale)",
    y = "Country"
  ) +
  theme_minimal(base_size = 14)

model_times <- data.frame(
  country = names(df_by_country),
  user = NA_real_,
  system = NA_real_,
  elapsed = NA_real_,
  stringsAsFactors = FALSE
)

models <- list()
for (i in seq_along(df_by_country)) {
  country_name <- names(df_by_country)[i]
  dat <- df_by_country[[i]]
  
  t <- system.time({
    m <- fit_country_model(dat)
  })
  
  models[[country_name]] <- m
  model_times$user[i]    <- t[["user.self"]]
  model_times$system[i]  <- t[["sys.self"]]
  model_times$elapsed[i] <- t[["elapsed"]]
}

print(model_times)

### b. ###
t_fast <- system.time({
  df_std <- df %>%
    group_by(country) %>%
    mutate(
      prior_gpa_z     = as.numeric(scale(prior_gpa)),
      forum_posts_z   = as.numeric(scale(forum_posts)),
      quiz_attempts_z = as.numeric(scale(quiz_attempts))
    ) %>%
    ungroup()
  
  df_by_country <- split(df_std, df_std$country)
  
  fit_and_extract <- function(dat) {
    m <- glmer(
      completed_course ~ prior_gpa_z + forum_posts_z + quiz_attempts_z +
        (1 | device_type),
      data = dat,
      family = binomial,
      control = glmerControl(optimizer = "bobyqa")
    )
    
    cf   <- fixef(m)
    est  <- unname(cf["forum_posts_z"])
    se   <- sqrt(diag(vcov(m)))["forum_posts_z"]
    
    list(est = est, se = se)
  }
  
  num_cores <- max(1, detectCores() %/% 2)
  cl <- makeCluster(num_cores)
  on.exit(stopCluster(cl), add = TRUE)
  clusterEvalQ(cl, { library(lme4); NULL })
  
  clusterExport(
    cl,
    varlist = c("df_by_country", "fit_and_extract"),
    envir = environment()
  )
  
  countries <- names(df_by_country)
  
  res_list <- parLapply(cl, countries, function(ctry) {
    dat <- df_by_country[[ctry]]
    out <- fit_and_extract(dat)
    data.frame(
      country = ctry,
      est     = out$est,
      se      = out$se,
      stringsAsFactors = FALSE
    )
  })
  
  coef_fast <- do.call(rbind, res_list)
  coef_fast <- coef_fast[order(coef_fast$country), ]
})

t_fast
coef_fast

##### Problem 4. #####




#Resources used: R documentation for unname() function and bootstrapping assistance, ChatGPT for debugging