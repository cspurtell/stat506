##### Problem 1 #####

### a. ###
abalones <- read.csv("data/abalone.data")
colnames(abalones) <- c("Sex", 
                        "Length", 
                        "Diameter", 
                        "Height", 
                        "Whole.weight",
                        "Shucked.weight",
                        "Viscera.weight",
                        "Shell.weight",
                        "Rings")


### b. ###
table(abalones$Sex)
# F    I    M 
# 1307 1342 1527


### c. ###
weights <- abalones[, c("Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight", "Rings")]
cor(weights)
# It appears that shell weight has the highest correlation with rings

abs_m <- subset(abalones, Sex == "M")
abs_f <- subset(abalones, Sex == "F")
abs_i <- subset(abalones, Sex == "I")
cor(abs_m$Shell.weight, abs_m$Rings) #0.5124437
cor(abs_f$Shell.weight, abs_f$Rings) #0.405907
cor(abs_i$Shell.weight, abs_i$Rings) #0.7254357
# It appears that for shell weight, infant has the highest correlation with rings

oldest <- subset(abalones, Rings == max(abalones$Rings))
oldest[, c("Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight")]
# Whole.weight  Shucked.weight  Viscera.weight  Shell.weight
# 1.8075        0.7055          0.3215          0.475

abalones$Is.larger <- abalones$Viscera.weight > abalones$Shell.weight
count_larger <- sum(abalones$Is.larger)
(count_larger / nrow(abalones)) * 100
# 6.51341% have a viscera weight larger than their shell weight


### d. ###
weights <- c("Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight")
abs_split <- split(abalones, abalones$Sex) #Splits into three separate data frames by sex
cor_table <- sapply(abs_split, function(sub) { #Applies function to each sub data frame
  sapply(sub[weights], function(x) cor(x, sub$Rings)) #Calculates the correlation values for each weight column
})
cor_table <- t(cor_table) #Makes rows the sexes
cor_table <- data.frame(Sex = rownames(cor_table), cor_table, row.names = NULL)
cor_table


### e. ###
t.test(abalones$Rings[abalones$Sex == "M"], abalones$Rings[abalones$Sex == "F"])
# Welch Two Sample t-test
# 
# data:  abalones$Rings[abalones$Sex == "M"] and abalones$Rings[abalones$Sex == "F"]
# t = -3.69, df = 2741.8, p-value = 0.0002286
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.6533201 -0.1999174
# sample estimates:
# mean of x   mean of y 
# 10.70269    11.12930

t.test(abalones$Rings[abalones$Sex == "M"], abalones$Rings[abalones$Sex == "I"])
# Welch Two Sample t-test
#
# data:  abalones$Rings[abalones$Sex == "M"] and abalones$Rings[abalones$Sex == "I"]
# t = 27.194, df = 2857.9, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   2.609451 3.014995
# sample estimates:
# mean of x   mean of y 
# 10.702685   7.890462

t.test(abalones$Rings[abalones$Sex == "F"], abalones$Rings[abalones$Sex == "I"])
# Welch Two Sample t-test
# 
# data:  abalones$Rings[abalones$Sex == "F"] and abalones$Rings[abalones$Sex == "I"]
# t = 29.477, df = 2508.9, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   3.023380 3.454304
# sample estimates:
# mean of x   mean of y 
# 11.129304   7.890462


##### Problem 2 #####

### a. ###
food_exp <- read.csv("data/food_expenditure.csv")


### b. ###
colnames(food_exp) <- c("ID",
                        "age",
                        "household_size",
                        "state",
                        "currency",
                        "total_expenditure",
                        "grocery_expenditure",
                        "dining_expenditure",
                        "misc_expenditure",
                        "times_dined",
                        "alcohol_included",
                        "assistance_programs")


### c. ###
food_usd <- food_exp[food_exp$currency == "USD", ]
cat("Before restriction: ", nrow(food_exp), " observations\n")
cat("After restriction: ", nrow(food_usd), " observations\n")


### d. ###
food_exp <- food_exp[food_exp$age >= 18 & food_exp$age <= 100, ]
# Excluded all minors as they are unlikely to be responsible for food spending, as well as implausible ages


### e. ###
valid_states <- state.abb #Pre-made vector of all 50 U.S states
valid_states <- c(valid_states, "DC")
food_exp <- food_exp[food_exp$state %in% valid_states, ]
# Removed invalid or missing entries for state name abbreviations


### f. ###
food_exp <- food_exp[!is.na(food_exp$total_expenditure) & food_exp$total_expenditure >= 0, ]
food_exp <- food_exp[is.na(food_exp$grocery_expenditure) | (food_exp$grocery_expenditure >= 0), ]
food_exp <- food_exp[is.na(food_exp$dining_expenditure) | (food_exp$dining_expenditure >= 0), ]
food_exp <- food_exp[is.na(food_exp$misc_expenditure) | (food_exp$misc_expenditure >= 0), ]
# Removed all instances of negative and missing values for total expenditure, but allowed NA to be a valid response for any of the subcategories/types of expenditures


### g. ###
food_exp <- food_exp[
  (food_exp$times_dined > 0 & (is.na(food_exp$dining_expenditure) | food_exp$dining_expenditure >= 0)) |
  (food_exp$times_dined == 0 & (is.na(df_clean$dining_expenditure) | food_exp$dining_expenditure == 0)),
]
# Removed inconsistent/implausible responses between number of reported times dined out and dining expenditures


### h. ###
nrow(food_exp)
# 181 rows


##### Problem 3 #####
#' Compute the next number in the Collatz sequence
#' 
#' @param n A positive number
#' @ return The next positive integer in the Collatz sequence
nextCollatz <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != floor(n)) {
    stop("Input must be a single positive integer")
  }
  
  if (n %% 2 == 0) {
    return(n / 2)
  } else {
    return(3 * n + 1)
  }
}

nextCollatz(5)
# [1] 16
nextCollatz(16)
# [1] 8


### b. ###
#' Compute the full Collatz sequence for a given integer
#' 
#' @param n A positive integer
#' @return A list containing:
#'            sequence: a vector of the Collatz sequence from n to 1
#'            length: the length of the above sequence
collatzSequence <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != floor(n)) {
    stop("Input must be a single positive integer")
  }
  
  seq_vec <- n
  while (seq_vec[length(seq_vec)] != 1) {
    seq_vec <- c(seq_vec, nextCollatz(seq_vec[length(seq_vec)]))
  }
  
  return(list(
    sequence = seq_vec,
    length = length(seq_vec)
  ))
}

collatzSequence(5)
# $sequence
# [1]  5 16  8  4  2  1

# $length
# [1] 6

collatzSequence(19)
# $sequence
# [1] 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10  5 16  8  4  2  1

# $length
# [1] 2


### c. ###






#Resources used: R documentation for subset(), split(), and cat() functions, Stack Overflow for Collatz function help