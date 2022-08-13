install.packages('swirl')

library(swirl)

install_course_github('NickCH-K','Econometrics')

swirl()

---------------------------------------------------------------
# 4: Hypothesis Testing

# Load Libraries
library(tidyverse)
library(fixest)

# Create the data with tibble() and mutate()
df <- tibble(X = runif(300)) %>% mutate(Y = 5 + 3*X + rnorm(300))

# What is the *true* value of beta1 (the effect of a one-unit increase in X on Y)?
# 3 

# Regression of Y on X using df
one_samp <- feols(Y~X, data = df)

# You can then get the specific coefficient you want using
# [['variablename']] . Store the coefficient on X as beta1:
beta1 <- coef(one_samp)[['X']]

# Standard error of beta1
se_beta1 <- one_samp$se

# beta1 is 3.121093

# beta1 is not exactly 3. What is this an example of?
# Sampling variation

# | We are going to perform a hypothesis test on beta1, which
# was estimated from a sample of 300 observations. What would
# be the most appropriate null distribution to use?
# Normal distribution

# Keeping in mind that you've already got the coefficient
# saved as beta1 and the standard error saved as se_beta1,
# calculate the z-score for a test of whether beta1 = 3.
(beta1 - 3)/se_beta1 
# 0.5944748

# For a 95% confidence level and a two-sided test, to reject
# the null you want 5% in *both tails*, meaning how much in
# *just one* tail?
# 0.025

# Use this to find the proportion to the right of the absolute
# value of your z-score.
1-pnorm(abs(z))

1-pnorm(abs((beta1-3)/se_beta1))
# 0.2760973

# So then, keeping mind that this is a two-tailed test so we
# want the proportion as far away from the null as your
# z-score, or farther, on *both sides*, what is the p-value of
# this test?
2*(1-pnorm(abs((beta1-3)/se_beta1)))
0.5521946

# Can you reject the null that beta1 = 3 at the 95% level with
# a two-sided test? TRUE

# Use data(storms)
data(storms)

# Use vtable to examine data
vtable::vtable(storms)

help(storms)

# Regression of wind on longtitude
wind_reg <- feols(wind ~ long, data = storms)

etable(wind_reg)

# What is the standard error on the coefficient on longitude?
0.0134

# Use coefplot() to look at the 95% confidence interval of the
# coefficient on longitude (called long in the regression).
coefplot(wind_reg, keep = 'long')

# Use wald() from fixest to test whether the coefficient on
# longitude is 0.
wald(wind_reg, 'long')

# Load libraries
install.packages("multcamp")
library(multcomp)

# Use glht()
wind_reg %>% glht('long = 5') %>% summary()

feols(wind~long, data = storms, vcov = 'hetero') %>% glht('long = 5') %>% summary()
