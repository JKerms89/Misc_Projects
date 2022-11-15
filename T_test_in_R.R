# Unpaired Two-Samples T-test in R

library(ggpubr)
library(dplyr)

# Data in two numeric vectors
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 
# Create a data frame
my_data <- data.frame( 
  group = rep(c("Woman", "Man"), each = 9),
  weight = c(women_weight,  men_weight)
)

# We want to know, if the average women's weight differs from the 
# average men's weight?

print(my_data)

#It's possible to compute summary statistics (mean and sd) by groups. 
#The dplyr package can be used.

group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

# Plot weight by group and color by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

#Preliminary test to check independent t-test assumptions
# Assumption 1: Are the two samples independents?
# Yes, since the samples from men and women are not related.

#Assumtion 2: Are the data from each of the 2 groups
# follow a normal distribution?

#Use Shapiro-Wilk normality test as described at: Normality Test in R. - Null hypothesis: the data are normally distributed - Alternative hypothesis: the data are not normally distributed
# We'll use the functions with() and shapiro.test() to 
# compute Shapiro-Wilk test for each group of samples.

# Shapiro-Wilk normality test for Men's weights
with(my_data, shapiro.test(weight[group == "Man"]))# p = 0.1

# Shapiro-Wilk normality test for Women's weights
with(my_data, shapiro.test(weight[group == "Woman"])) # p = 0.6

#From the output, the two p-values are greater than the significance 
#level 0.05 implying that the distribution of the data are not 
#significantly different from the normal distribution. 
# In other words, we can assume the normality.

# Assumption 3. Do the two populations have the same variances?
res.ftest <- var.test(weight ~ group, data = my_data)
res.ftest

#The p-value of F-test is p = 0.1713596. It's greater than the 
# significance level alpha = 0.05. In conclusion, there is no 
# significant difference between the variances of the two sets 
# of data. Therefore, we can use the classic t-test witch 
# assume equality of the two variances.

#Compute unpaired two-samples t-test
# Question : Is there any significant difference between 
# women and men weights?


# Compute t-test
res <- t.test(women_weight, men_weight, var.equal = TRUE)
res

# In the result above :
  
#- t is the t-test statistic value (t = 2.784),
#- df is the degrees of freedom (df= 16),
#- p-value is the significance level of the t-test (p-value = 0.01327).
#- conf.int is the confidence interval of the mean at 95% (conf.int = [4.0298, 29.748]);
#- sample estimates is he mean value of the sample (mean = 68.9888889, 52.1).

# Note that:
  
# if you want to test whether the average men's weight is less 
# than the average women's weight, type this:

t.test(weight ~ group, data = my_data,
       var.equal = TRUE, alternative = "less")

# Or, if you want to test whether the average men's weight
# is greater than the average women's weight, type this:

t.test(weight ~ group, data = my_data,
       var.equal = TRUE, alternative = "greater")

# Interpretation of result

#The p-value of the test is 0.01327, which is less than the 
#significance level alpha = 0.05. We can conclude that men's 
#average weight is significantly different from women's average 
#weight with a p-value = 0.01327.

#The result of t.test() function is a list containing the following components:
  
  
# statistic: the value of the t test statistics
# parameter: the degrees of freedom for the t test statistics
# p.value: the p-value for the test
# conf.int: a confidence interval for the mean appropriate to the specified alternative hypothesis.
# estimate: the means of the two groups being compared (in the case of independent t test) or difference in means (in the case of paired t test).

# printing the p-value
res$p.value

# printing the mean
res$estimate

# printing the confidence interval
res$conf.int

