#Install packages for ANOVA

install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))

# Import

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(readr)

# Import dataset

crop.data <- read.csv("C:/Users/kerme/Downloads/crop.data.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric"))

# Check summary of data
summary(crop.data)

#In the one-way ANOVA example, we are modeling crop yield as a 
#function of the type of fertilizer used. First we will use aov() 
#to run the model, then we will use summary() to print the summary 
# of the model.

one.way <- aov(yield ~ fertilizer, data = crop.data)
summary(one.way)

#The p-value of the fertilizer variable is low (p < 0.001), so it 
#appears that the type of fertilizer used has a real impact on 
#the final crop yield.

#Two-way ANOVA

two.way <- aov(yield ~ fertilizer + density, data = crop.data)
summary(two.way)

#Adding planting density to the model seems to have made the model 
#better: it reduced the residual variance (the residual sum of 
#squares went from 35.89 to 30.765), and both planting density
#and fertilizer are statistically significant (p-values < 0.001).

#Sometimes you have reason to think that two of your independent 
#variables have an interaction effect rather than an additive effect.
#For example, in our crop yield experiment, it is possible that 
#planting density affects the plants' ability to take up fertilizer. This might influence the effect of fertilizer type in a way that isn't accounted for in the two-way model.
#To test whether two variables have an interaction effect in ANOVA, 
#simply use an asterisk instead of a plus-sign in the model:

interaction <- aov(yield ~ fertilizer*density, data = crop.data)
summary(interaction)

#In the output table, the 'fertilizer:density' variable has a low 
#sum-of-squares value and a high p-value, which means there is not 
#much variation that can be explained by the interaction between 
#fertilizer and planting density.

#Adding a blocking variable
#If you have grouped your experimental treatments in some way, or if 
#you have a confounding variable that might affect the relationship 
#you are interested in testing, you should include that element 
#in the model as a blocking variable. The simplest way to do this 
#is just to add the variable into the model with a '+'.
#For example, in many crop yield studies, treatments are applied 
#within 'blocks' in the field that may differ in soil texture, 
#moisture, sunlight, etc. To control for the effect of differences 
#among planting blocks we add a third term, 'block', to our ANOVA.

blocking <- aov(yield ~ fertilizer + density + block, data = crop.data)
summary(blocking)

#The 'block' variable has a low sum-of-squares value (0.486) and a 
#high p-value (p = 0.48), so it's probably not adding 
#much information to the model. It also doesn't change the sum of 
#squares for the two independent variables, which means that it's 
#not affecting how much variation in the dependent variable they explain.

model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)

#From these results, it appears that the two.way model is the best 
#fit. The two-way model has the lowest AIC value, and 71% of the AIC
#weight, which means that it explains 71% of the total variation in
#the dependent variable that can be explained by the full set of models.
#The model with blocking term contains an additional 15% of the AIC 
#weight, but because it is more than 2 delta-AIC worse than the best
#model, it probably isn't good enough to include in your results.

#To check whether the model fits the assumption of homoscedasticity,
#look at the model diagnostic plots in R using the plot() function:

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#Step 5: Do a post-hoc test
#ANOVA tells us if there are differences among group means, but 
#not what the differences are. To find out which groups are 
#statistically different from one another, you can perform a 
#Tukey's Honestly Significant Difference (Tukey's HSD) post-hoc 
#test for pairwise comparisons:

tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

#From the post-hoc test results, we see that there are 
#statistically significant differences (p < 0.05) between 
#fertilizer groups 3 and 1 and between fertilizer types 3 and 2, 
#but the difference between fertilizer groups 2 and 1 is not 
#statistically significant. There is also a significant 
#difference between the two different levels of planting density.

#Step 6: Plot the results in a graph
#When plotting the results of a model, it is important to display:
  
#- the raw data
# - summary information, usually the mean and standard error of 
# each group being compared.
# - letters or symbols above each group being compared to indicate 
# the groupwise differences.

# Find the groupwise differences
# From the ANOVA test we know that both planting density and 
# fertilizer type are significant variables. To display this 
# information on a graph, we need to show which of the combinations of fertilizer type + planting density are statistically different from one another.
# To do this, we can run another ANOVA + TukeyHSD test, this time 
# using the interaction of fertilizer and planting density. 
# We aren't doing this to find out if the interaction term is 
# significant (we already know it's not), but rather to find out 
# which group means are statistically different from one another 
# so we can add this information to the graph.

tukey.plot.aov<-aov(yield ~ fertilizer:density, data=crop.data)

#Instead of printing the TukeyHSD results in a table, we'll do it in a graph.

tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#The significant groupwise differences are any where the 95% 
#confidence interval doesn't include zero. This is another way of 
#saying that the p-value for these pairwise differences is < 0.05.
#From this graph, we can see that the fertilizer + planting density
#combinations which are significantly different from one another 
#are 3:1-1:1 (read as "fertilizer type three + planting density 1 
#contrasted with fertilizer type 1 + planting density type 1"), 
#1:2-1:1, 2:2-1:1, 3:2-1:1, and 3:2-2:1.
#We can make three labels for our graph: A (representing 1:1), B 
#(representing all the intermediate combinations), and C 
#(representing 3:2).

mean.yield.data <- crop.data %>%
  group_by(fertilizer, density) %>%
  summarise(
    yield = mean(yield)
  )

#Next, add the group labels as a new variable in the data frame.

mean.yield.data$group <- c("a","b","b","b","b","c")
mean.yield.data

#Now we are ready to start making the plot for our report.
#Plot the raw data

two.way.plot <- ggplot(crop.data, aes(x = density, y = yield, group=fertilizer)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

two.way.plot


#Add the means and standard errors to the graph

two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.yield.data, aes(x=density, y=yield))

two.way.plot

#This is very hard to read, since all of the different groupings for 
#fertilizer type are stacked on top of one another. We will solve 
#this in the next step.

#Split up the data

#To show which groups are different from one another, use facet_wrap()
#to split the data up over the three types of fertilizer. To add 
#labels, use geom_text(), and add the group letters from 
#the mean.yield.data dataframe you made earlier.

two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$group, vjust = -8, size = 5) +
  facet_wrap(~ fertilizer)

two.way.plot

#Make the graph ready for publication
#In this step we will remove the grey background and 
#add axis labels.

two.way.plot <- two.way.plot +
  theme_classic2() +
  labs(title = "Crop yield in response to fertilizer mix and planting density",
       x = "Planting density (1=low density, 2=high density)",
       y = "Yield (bushels per acre)")

two.way.plot

#Step 7: Report the results
#In addition to a graph, it's important to state the results of the ANOVA test. Include:
  
# - A brief description of the variables you tested
# - The f-value, degrees of freedom, and p-values for each independent variable
# - What the results mean.

# Example

# We found a statistically-significant difference in average crop 
# yield by both fertilizer type (f(2)=9.018, p < 0.001) and by 
#planting density (f(1)=15.316, p<0.001).
# A Tukey post-hoc test revealed that fertilizer mix 3 resulted 
# in a higher yield on average than fertilizer mix 1 
# (0.59 bushels/acre), and a higher yield on average than fertilizer 
# mix 2 (0.42 bushels/acre). Planting density was also significant, 
# with planting density 2 resulting in an higher yield on average 
# of 0.46 bushels/acre over planting density 1.
# A subsequent groupwise comparison showed the strongest yield gains 
# at planting density 2, fertilizer mix 3, suggesting that this mix 
# of treatments was most advantageous for crop growth under our 
# experimental conditions.












