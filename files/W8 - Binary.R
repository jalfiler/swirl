install.packages('swirl')

library(swirl)

install_course_github('NickCH-K','Econometrics')

swirl()

---------------------------------------------------------------
# 8: Binary Dependent Variables

# Load Libraries
library(tidyverse)
library(vtable)
library(fixest)
library(marginaleffects)


# Load in data
data(TitanicSurvival, package = 'carData')

# Let's take a look at the data. First, use vtable() to look at the variables and
# variable types in the data. Include the lush = TRUE option.
vtable(TitanicSurvival, lush = TRUE)


# drop any rows with missing data in any variable.
TitanicSurvival <- TitanicSurvival %>% na.omit()

# So, use mutate to create a new variable surv_TF equal to survived == 'yes' and
# overwrite TitanicSurvival.
TitanicSurvival <- TitanicSurvival %>% mutate(surv_TF = survived == 'yes')

# Okay! Now use feols() to regress surv_TF on the other variables in the data. Leave out
# se = 'hetero' for now, we'll add it in etable.
lpm <- feols(surv_TF ~ sex + age + passengerClass, data = TitanicSurvival)

etable(lpm, se = 'hetero')

logit <- feglm(surv_TF ~ age + sex + passengerClass, data = TitanicSurvival, family = binomial(link = 'logit'))

etable(logit, se = 'hetero')

TitanicSurvival <- TitanicSurvival %>% mutate(response_predict = survived == 'yes')

loglink <- function(x) { exp(x)/(1+exp(x)) }

margfx <- summary(marginaleffects(logit))

wald(logit, 'passengerClass')

























