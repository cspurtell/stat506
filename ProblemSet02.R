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

### d. ###
simulate_prob <- function(n, trials = 10000) {
  results <- replicate(trials, random_walk1(n) == 0)
  successes <- sum(results)   # count how many ended at 0
  prop.test(successes, trials)
}

simulate_prob(10)
# 1-sample proportions test with continuity correction

# data:  successes out of trials, null probability 0.5
# X-squared = 5280.9, df = 1, p-value < 2.2e-16
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#   0.1299594 0.1435217
# sample estimates:
#  p 
# 0.1366

simulate_prob(100)
# 1-sample proportions test with continuity correction

# data:  successes out of trials, null probability 0.5
# X-squared = 9164.2, df = 1, p-value < 2.2e-16
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#   0.01860181 0.02437231
# sample estimates:
#   p 
# 0.0213

simulate_prob(1000)
# 1-sample proportions test with continuity correction

# data:  successes out of trials, null probability 0.5
# X-squared = 9806.9, df = 1, p-value < 2.2e-16
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
# 0.003579479 0.006414732
# sample estimates:
#   p 
# 0.0048


##### Problem 2 #####
simulate_daily_cars <- function(trials = 100000, seed = 1) {
  if (!is.null(seed)) set.seed(seed)

  totals <- rowSums(cbind(
    replicate(8, rpois(trials, 1)),           # midnight-7AM
    rnorm(trials, 60, sqrt(12)),              # 8AM
    replicate(8, rpois(trials, 8)),           # 9AM-4PM
    rnorm(trials, 60, sqrt(12)),              # 5PM
    replicate(6, rpois(trials, 12))           # 6PM-11PM
  ))
  
  mean_daily <- mean(totals)
  se_mean <- sd(totals) / sqrt(trials)
  ci95 <- mean_daily + c(-1,1) * qnorm(0.975) * se_mean
  
  list(mean_daily = mean_daily, se_mean = se_mean, ci95_mean = ci95)
}

simulate_daily_cars()
# $mean_daily
# [1] 263.9983

# $se_mean
# [1] 0.0409791

# $ci95_mean
# [1] 263.9180 264.0787


##### Problem 3 #####
youtube <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

### a. ###
#Need to remove year, brand, superbowl url, youtube url, id, etag, published_at, title, description, thumbnail, channel title
youtube_sub <- subset(youtube, select = -c(brand, superbowl_ads_dot_com_url, youtube_url, id, etag, published_at, title, description, thumbnail,
  channel_title)
)

dim(youtube_sub)
# [1] 247  15

### b. ###
hist(youtube_sub$view_count, main="Views", breaks=30) #Viable, but needs log transformation
youtube_sub$log_view_count <- log1p(youtube_sub$view_count)
hist(youtube_sub$log_view_count, main="Views", breaks=30) #Variable can now be used 

hist(youtube_sub$like_count, main="Like Count", xlab="", breaks=30) #Viable, but needs log transformation
youtube_sub$log_like_count <- log1p(youtube_sub$like_count)
hist(youtube_sub$log_like_count, main="Like Count", xlab="", breaks = 30) #Variable can now be used

hist(youtube_sub$dislike_count, main="Dislike Count", xlab="", breaks=30) #Viable, but needs log transformation
youtube_sub$log_dislike_count <- log1p(youtube_sub$dislike_count)
hist(youtube_sub$log_dislike_count, main="Dislike Count", xlab="", breaks=30) #Variable can now be used

hist(youtube_sub$favorite_count, main="Favorite Count", xlab="", breaks=30) #Not viable

hist(youtube_sub$comment_count, main="Comment Count", xlab="", breaks=30) #Viable, but needs log transformation
youtube_sub$log_comment_count <- log1p(youtube_sub$comment_count)
hist(youtube_sub$log_comment_count, main="Comment Count", xlab="", breaks=30) #Variable can now be used

### c. ###
model_views <- lm(log_view_count ~ funny + show_product_quickly + patriotic + 
                    celebrity + danger + animals + use_sex + year, 
                  data = youtube_sub)
summary(model_views)

# Call:
#   lm(formula = log_view_count ~ funny + show_product_quickly + 
#        patriotic + celebrity + danger + animals + use_sex + year, 
#      data = youtube_sub)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.7742 -1.6152  0.1311  1.7036  8.4481 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)              -31.55016   71.00538  -0.444    0.657
# funnyTRUE                  0.56492    0.46702   1.210    0.228
# show_product_quicklyTRUE   0.21089    0.40530   0.520    0.603
# patrioticTRUE              0.50699    0.53811   0.942    0.347
# celebrityTRUE              0.03548    0.42228   0.084    0.933
# dangerTRUE                 0.63131    0.41812   1.510    0.132
# animalsTRUE               -0.31002    0.39348  -0.788    0.432
# use_sexTRUE               -0.38671    0.44782  -0.864    0.389
# year                       0.02053    0.03531   0.582    0.561

# Residual standard error: 2.787 on 222 degrees of freedom
# (16 observations deleted due to missingness)
# Multiple R-squared:  0.02694,	Adjusted R-squared:  -0.008122 
# F-statistic: 0.7684 on 8 and 222 DF,  p-value: 0.631

model_likes <- lm(log_like_count ~ funny + show_product_quickly + patriotic + 
                    celebrity + danger + animals + use_sex + year, 
                  data = youtube_sub)
summary(model_likes)

# Call:
#   lm(formula = log_like_count ~ funny + show_product_quickly + 
#        patriotic + celebrity + danger + animals + use_sex + year, 
#      data = youtube_sub)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.2860 -1.6333  0.0868  1.4911  7.7431 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)              -150.51357   63.45723  -2.372   0.0186 *
#   funnyTRUE                   0.47476    0.41816   1.135   0.2575  
# show_product_quicklyTRUE    0.20017    0.36391   0.550   0.5828  
# patrioticTRUE               0.80689    0.49791   1.621   0.1066  
# celebrityTRUE               0.41283    0.38212   1.080   0.2812  
# dangerTRUE                  0.63895    0.37350   1.711   0.0886 .
# animalsTRUE                -0.05944    0.35298  -0.168   0.8664  
# use_sexTRUE                -0.42952    0.40064  -1.072   0.2849  
# year                        0.07685    0.03155   2.436   0.0157 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 2.467 on 216 degrees of freedom
# (22 observations deleted due to missingness)
# Multiple R-squared:  0.07313,	Adjusted R-squared:  0.03881 
# F-statistic:  2.13 on 8 and 216 DF,  p-value: 0.0342

model_dislikes <- lm(log_dislike_count ~ funny + show_product_quickly + patriotic + 
                    celebrity + danger + animals + use_sex + year, 
                  data = youtube_sub)
summary(model_dislikes)

# Call:
#   lm(formula = log_dislike_count ~ funny + show_product_quickly + 
#        patriotic + celebrity + danger + animals + use_sex + year, 
#      data = youtube_sub)
#
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.0292 -1.3299 -0.3192  0.8986  8.7219 
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              -183.06813   53.34768  -3.432 0.000719 ***
#   funnyTRUE                   0.25949    0.35154   0.738 0.461224    
# show_product_quicklyTRUE    0.27511    0.30593   0.899 0.369515    
# patrioticTRUE               0.81407    0.41859   1.945 0.053095 .  
# celebrityTRUE              -0.20214    0.32125  -0.629 0.529852    
# dangerTRUE                  0.22184    0.31400   0.707 0.480630    
# animalsTRUE                -0.21192    0.29675  -0.714 0.475911    
# use_sexTRUE                -0.32980    0.33681  -0.979 0.328583    
# year                        0.09207    0.02653   3.471 0.000626 ***
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.074 on 216 degrees of freedom
# (22 observations deleted due to missingness)
# Multiple R-squared:  0.09753,	Adjusted R-squared:  0.06411 
# F-statistic: 2.918 on 8 and 216 DF,  p-value: 0.004115

model_comments <- lm(log_comment_count ~ funny + show_product_quickly + patriotic + 
                       celebrity + danger + animals + use_sex + year, 
                     data = youtube_sub)
summary(model_comments)

# Call:
#   lm(formula = log_comment_count ~ funny + show_product_quickly + 
#        patriotic + celebrity + danger + animals + use_sex + year, 
#      data = youtube_sub)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.1372 -1.4665 -0.1427  1.4061  5.8468 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)              -99.09835   52.92351  -1.872   0.0625 .
# funnyTRUE                  0.21954    0.34528   0.636   0.5256  
# show_product_quicklyTRUE   0.40939    0.30229   1.354   0.1771  
# patrioticTRUE              0.66698    0.39902   1.672   0.0961 .
# celebrityTRUE              0.29767    0.31541   0.944   0.3464  
# dangerTRUE                 0.17784    0.31069   0.572   0.5677  
# animalsTRUE               -0.26802    0.29347  -0.913   0.3621  
# use_sexTRUE               -0.39323    0.33163  -1.186   0.2370  
# year                       0.05034    0.02632   1.913   0.0571 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 2.039 on 213 degrees of freedom
# (25 observations deleted due to missingness)
# Multiple R-squared:  0.06535,	Adjusted R-squared:  0.03025 
# F-statistic: 1.862 on 8 and 213 DF,  p-value: 0.06748


### d. ###
y <- youtube_sub$log_view_count
X <- model.matrix(~ funny + show_product_quickly + patriotic +
                    celebrity + danger + animals + use_sex + year, 
                  data = youtube_sub)
complete_idx <- complete.cases(X, y)
X <- X[complete_idx, ]
y <- y[complete_idx]

beta_hat <- solve(crossprod(X)) %*% crossprod(X, y)
beta_hat
# [,1]
# (Intercept)              -31.55015804
# funnyTRUE                  0.56492445
# show_product_quicklyTRUE   0.21088918
# patrioticTRUE              0.50699051
# celebrityTRUE              0.03547862
# dangerTRUE                 0.63131085
# animalsTRUE               -0.31001838
# use_sexTRUE               -0.38670726
# year                       0.02053399


#Resources Used: R documentation for model.matrix() function, ChatGPT for debugging