# One-Sample T-test in R
# http://www.sthda.com/english/wiki/one-sample-t-test-in-r 

# one-sample t-test is used to compare the mean of one sample to a 
# known standard (or theoretical/hypothetical) mean (??).

#Generally, the theoretical mean comes from:
  
# - a previous experiment. For example, compare whether the mean weight of mice differs from 200 mg, a value determined in a previous study.
# - or from an experiment where you have control and treatment conditions. If you express your data as "percent of control", you can test whether the average value of treatment condition differs significantly from 100.

# Note that, one-sample t-test can be used only, when the data are 
# normally distributed . This can be checked using Shapiro-Wilk test.

# Visualize your data and compute one-sample t-test in R
# Install ggpubr R package for data visualization
# You can draw R base graps as described at this link: 
# R base graphs. Here, we'll use the ggpubr R package for an 
# easy ggplot2-based data visualization

library(ggpubr)

# Here, we'll use an example data set containing 
# the weight of 10 mice.

# We want to know, if the average weight of the mice
# differs from 25g?

set.seed(1234)
my_data <- data.frame(
  name = paste0(rep("M_", 10), 1:10),
  weight = round(rnorm(10, 20, 2), 1)
)

# Print the first 10 rows of the data
head(my_data, 10)

# Statistical summaries of weight
summary(my_data$weight)

ggboxplot(my_data$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

# Preliminary test to check one-sample t-test assumptions
# Is this a large sample? - No, because n < 30.
# Since the sample size is not large enough (less than 30, 
# central limit theorem), we need to check whether the data 
# follow a normal distribution.

# How to check the normality?
  
# Read this article: Normality Test in R.

# Briefly, it's possible to use the Shapiro-Wilk normality test and to look at the normality plot.

# Shapiro-Wilk test:
# - Null hypothesis: the data are normally distributed
# - Alternative hypothesis: the data are not normally distributed

shapiro.test(my_data$weight) # => p-value = 0.6993

# From the output, the p-value is greater than the significance 
# level 0.05 implying that the distribution of the data are not 
# significantly different from normal distribtion. 
# In other words, we can assume the normality.

# Visual inspection of the data normality using Q-Q plots 
# (quantile-quantile plots). Q-Q plot draws the correlation 
# between a given sample and the normal distribution.

ggqqplot(my_data$weight, ylab = "Men's weight",
         ggtheme = theme_minimal())


# From the normality plots, we conclude that the data may 
# come from normal distributions.
#Note that, if the data are not normally distributed, 
#it's recommended to use the non parametric one-sample Wilcoxon 
# rank test.

# Compute one-sample t-test
# We want to know, if the average weight of the mice 
# differs from 25g (two-tailed test)?

# One-sample t-test
res <- t.test(my_data$weight, mu = 25)
# Printing the results
res 

# In the result above :
  
# - t is the t-test statistic value (t = -9.078),
# - df is the degrees of freedom (df= 9),
# - p-value is the significance level of the t-test (p-value = 7.95310^{-6}).
# - conf.int is the confidence interval of the mean at 95% (conf.int = [17.8172, 20.6828]);
# - sample estimates is he mean value of the sample (mean = 19.25)

# Note that:
  
# if you want to test whether the mean weight of mice is less than 25g (one-tailed test), type this:
  t.test(my_data$weight, mu = 25,
         alternative = "less")
  
# Or, if you want to test whether the mean weight of mice is greater than 25g (one-tailed test), type this:
  t.test(my_data$weight, mu = 25,
         alternative = "greater")

# Interpretation of the result
# The p-value of the test is 7.95310^{-6}, which is less than 
# the significance level alpha = 0.05. We can conclude that the 
# mean weight of the mice is significantly different from 25g with
# a p-value = 7.95310^{-6}.
  
# Access to the values returned by t.test() function
# The result of t.test() function is a list containing the following components:
    
    
# - statistic: the value of the t test statistics
# - parameter: the degrees of freedom for the t test statistics
# - p.value: the p-value for the test
# - conf.int: a confidence interval for the mean appropriate to the specified alternative hypothesis.
# - estimate: the means of the two groups being compared 
# (in the case of independent t test) or difference in means 
# (in the case of paired t test).
  
# printing the p-value
  res$p.value

# printing the mean
  res$estimate
  
# printing the confidence interval
  res$conf.int
