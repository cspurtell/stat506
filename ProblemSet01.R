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














#Resources used: R documentation for subset() and split() functions, 