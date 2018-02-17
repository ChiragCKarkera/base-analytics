# R Studio is used

# worker's compensation claim data
claim<- read.csv(file="Claims.csv", header=TRUE, stringsAsFactors=TRUE)

# z scores
wage_z_score <- scale(claim$Average.Weekly.Wage, center = T, scale = T)

# simple random sample
library(dplyr)
set.seed(1545)
srs <- sample_n(claim,500)

# stratified random sampling by group 
library(splitstackshape)
set.seed(155)
sample_stratified <- stratified(claim, c("Claims_Category"),0.1)
View(sample_stratified)

# histogram
hist(wage_z_score, breaks = 50)

##### probability between values for normal distribution#####
# mean of weekly wage
avg_wage <- mean(claim$Average.Weekly.Wage)


# sandard deviation of wage
var_wage <- var(claim$Average.Weekly.Wage)
std_dev_wage <- sqrt(var_wage)



# probability of wage in bebtween 2000 to 30000
prob_wage_2k_3k <- pnorm(3000,avg_wage,std_dev_wage)-pnorm(2000,avg_wage,std_dev_wage)


########### t-test for population mean #############
# test for population mean if mean = 1140
t.test(claim$Average.Weekly.Wage, mu=1140, alternative = "two.sided", conf.level=0.95)

# conclusion - population mean equal to 1140

# t-test for population mean if mean = 1150
t.test(claim$Average.Weekly.Wage, mu=1150, alternative = "greater", conf.level=0.95) # can set "less" also
# conclusion - population mean equal 1150

######## chi-sq test for assume population variance = 846400. So, std dev =920 ###########
library("TeachingDemos")

sigma.test(claim$Average.Weekly.Wage, sigma = 920, alternative = "two.sided", conf.level = 0.95)
# Conclusion - std dev = 920



# test for normality
library(nortest)

# Anderson-Darling Normality Test
ad.test(claim$Average.Weekly.Wage)
# conclusion - data is not from a normal distribution

###### chi sq goodness of fit test for normality ######
# categorizing variable
h <- hist(wage_z_score, breaks = 6)

h
# getting theoretic probabilities for the categories of data
library("zoo")
breaks_cdf <- pnorm(h$breaks, mean = 0, sd =1 )
probs <- rollapply(breaks_cdf, 2, function(x) x[2]-x[1])

# chi sq test
chisq.test(h$counts, p=probs, rescale.p=TRUE, simulate.p.value=TRUE)
# conclusion - variable is not normally distributed

