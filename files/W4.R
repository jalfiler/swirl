install.packages('swirl')

library(swirl)

install_course_github('NickCH-K','Econometrics')

swirl()

---------------------------------------------------------------
# 4: Multivariate Regression

# Load Libraries
library(tidyverse)
library(NHANES)
library(fixest)

# Load Data
data(NHANES)
help(NHANES)

#  We are interested in the effect of poor sleep on your physical health.

# Use feols() to run a regression in the NHANES data, with
# DaysPhysHlthBad as the dependent variable and SleepHrsNight as
# the independent variable.

biv <- feols(DaysPhysHlthBad~SleepHrsNight, data = NHANES)

etable(biv)

# Regression
multi1 <- feols(DaysPhysHlthBad~SleepHrsNight + Education + Weight + Poverty, data = NHANES)

multi2 <- feols(DaysPhysHlthBad~SleepHrsNight + Depressed + HHIncomeMid + Age + TVHrsDay, data = NHANES)

etable(biv, multi2)

# Use coefplot(list(model1,model2)) to see all of the coefficients in both biv and multi2,
# and examine how much the coefficient on sleep hours is changed by adding the controls.
coefplot(list(biv,multi2))

# Use wald() to test whether Age = 0 AND HHIncomeMid = 0.
wald(multi2, c('Age','HHIncomeMid'))

wald(multi2, "TVHrsDay")

final <- feols(Y ~ X + D, data=example_dgp)

etable(final)
