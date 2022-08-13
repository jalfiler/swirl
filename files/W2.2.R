install.packages('swirl')

library(swirl)

install_course_github('NickCH-K','Econometrics')

swirl()
--------------------------------------------------------------------------------
# 2: Ordinary Least Squares Part 2
install.packages('Ecdat')

# Load libraries
library(tidyverse)
library(Ecdat)
library(vtable)
library(fixest)

# Load up MedExp data
data(MedExp)

help(MedExp)

# Take MedExp, pipe (%>%) it to the select function, and then list the variables you want to keep.
MedExp <- MedExp %>% select(age, med)

# Summary statistics
sumtable(MedExp) 
#or 
MedExp %>% sumtable()
  
# Graph
ggplot(MedExp, aes(x = age, y = med)) + geom_point()

# Logarithmic graph
ggplot(MedExp, aes(x = age, y = log(med))) + geom_point()

# drop
MedExp <- MedExp %>% filter(med != 0)

# Regress log(med) on age and save as med_age
med_age <- feols(log(med)~age, data = MedExp)
# slope: 0.02
# Interpretation: An age increase of 1 year is associated with a 0.02 increase in log(med).

# Predicted values and residuals
predict(med_age)

resid(med_age)^2 %>% sum() #Answer: 8905.144

MedExp <- MedExp %>% mutate(pred = predict(med_age))

# Graph
ggplot(MedExp, aes(x = age, y = pred)) + geom_point()

# issues in the error terms
tb <- 
  tibble(X = runif(100)) %>% 
  mutate(error = rnorm(100,mean = 1.5*X, sd = X)) %>% mutate(Y = .2*X + error)

# Create data
tb <- 
  tibble(X = runif(100)) %>% 
  mutate(error = rnorm(100,mean = 1.5*X, sd = X)) %>% mutate(Y = .2*X + error)

# Regression 
biased_reg <- feols(Y~X, data = tb)

# Graph plot
ggplot(tb, aes(x = X, y = Y)) + geom_point() + geom_smooth(method = 'lm')

# run etable()
etable(biased_reg, vcov = 'hetero')
