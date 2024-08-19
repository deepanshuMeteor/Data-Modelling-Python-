# Correlation analysis
######################

# T1
# set the work directory
setwd("c:\\EK\\work_data")

# read file cdc.csv
data <- read.csv("cdc.csv")

# T2
# Null hypothesis: there is no correlation between
# men's height and weight (r = 0)

# T3
# Alternative hypothesis: there is correlation between
# men's height and weight (r != 0)

# T4
# separate the data for male heights and weights
heights_m <- data[data$gender=="m","height"]
weights_m <- data[data$gender=="m","weight"]

# T5
length(heights_m)
length(weights_m)

str(heights_m)
str(weights_m)

summary(heights_m)
summary(weights_m)

par(mfrow=c(1,2))
hist(heights_m)
hist(weights_m)
## numeric continuous variables
## normally distributed

# T6
# scatter plot
plot(heights_m, weights_m)
## there is a linear upward trend
## plot the line of best fit
abline(lm(weights_m ~ heights_m), col="red", lwd=2)

# T7
# The cor() function calculates the correlation 
# coefficient but does not conduct any significance tests.

# We need to use cor.test()

#  If there is missing data, include parameter 
# use="complete.obs" in both cor() and cor.test(). 
cor(heights_m, weights_m)
## 0.43 - positive correlation, not strong
cor.test(heights_m, weights_m)
## p-value very small (less than 0.05)
## The null hypothesis is rejected
## The correlation is statistically significant
