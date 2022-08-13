install.packages('swirl')

library(swirl)

install_course_github('NickCH-K','Econometrics')

swirl()

--------------------------------------------------------------------------------
# 2: Ordinary Least Squares Part 1
install.packages(c('tidyverse','vtable','fixest')) 

#Load libraries
library(tidyverse)
library(vtable)
library(fixest)

# Load mtcars
data(mtcars)

# Use help(mtcars) for variable descriptions
help(mtcars)

View(mtcars)

# URL
"https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/USFinanceIndustry.csv"

# Data file
df <- read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/USFinanceIndustry.csv")
                     
feols(mpg ~ hp, data = mtcars)

lm(mpg ~ hp, data = mtcars) # to find slope of hp
# slope is -0.68. 
# Interpretation: An increase in hp of one unit is associated with a -.068 decrease in mpg.

# intercept: 30
# Interpretation: When hp = 0, we predict that mpg = 30.10.

my_model <- feols(mpg ~ hp, data = mtcars)

my_model

etable(my_model)

# Graph
ggplot(mtcars, aes(x = hp, y = mpg))
 + geom_point() + geom_smooth(method = 'lm')

# We are going to look at how corporate profits predict financial-sector profits.
reg2 <- feols(Financial ~ CorporateProfitsAdj, data = df)

# add new var. called ProfitsHalf that divides CorporateProfitsAdj by 2.
df2 <- df %>% mutate(ProfitsHalf = CorporateProfitsAdj/2)

# Regression of Financial on ProfitsHalf using df2.
reg3 <- feols(Financial ~ ProfitsHalf, data = df2)

# Regression 2 and 3 on etable()
etable(reg2)

etable(reg3)