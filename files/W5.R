install.packages('swirl')

library(swirl)

install_course_github('NickCH-K','Econometrics')

swirl()

---------------------------------------------------------------
# 5: Binary Variables and Functional Form

# Install Packages
install.packages(c('tidyverse','vtable','car','fixest'),dependencies = TRUE)

# Load Libraries
library(tidyverse)
library(vtable)
library(car)
library(fixest)

# Load Cowles data set.
data(Cowles)
vtable(Cowles) # look at the types of variables listed



# Use MUTATE to update Cowles with a new variable "male" that's TRUE 
# if sex == 'male' and FALSE otherwise. (add new variable male)
Cowles <- Cowles %>% mutate(male = sex == 'male') 

# Added a new variable column 'Volunteered', and if volunteer == yes then TRUE, otherwise no == FALSE.
Cowles <- Cowles %>% mutate(volunteered = volunteer == 'yes')


# Use feols() to perform a regression using volunteered and male that will tell us whether men or women are more
# likely to volunteer for psychological research. Store the regression as gender_diff.

gender_diff <- feols(volunteered ~ sex, data = Cowles)
# x = volunteered (dependent, effect)
# y = sex (independent, cause)

etable(gender_diff)

# Using the regression table, what proportion of WOMEN volunteer for psychological research? Put your
# answer as a number between 0 and 1 with four digits after the decimal place.

# 0.4474 *** (Intercept)

# Using the regression table, what proportion of MEN volunteer for psychological research? Put your answer
# as a number between 0 and 1 with four digits after the decimal place.

# 0.3869 (0.4474 - 0.0605 = 0.3869)

# Interpretation:  Women volunteer more than men, but it's not significant at 1%.



# Create a hair color variable:
Cowles <- Cowles %>% mutate(hair = c(rep('Brown',500),rep('Black',500),rep('Blonde',421)))

# Regress volunteered on hair and pass the result into export_summs.
feols(volunteered~hair, data = Cowles) %>% etable()

# Interpretation: The same proportion of blonde and black-haired people volunteered



# Use feols to regress volunteered on extraversion and store the result as m1.
m1 <- feols(volunteered ~ extraversion, data = Cowles)

m2 <- feols(volunteered ~ extraversion + I(extraversion^2), data = Cowles)

etable(m1, m2, digits = 3)

# Interpretation: The relationship between volunteering and extraversion is obviously positive. But the coefficient on
# extraversion in m2 is negative! Should we be concerned?
# No, because the individual coefficients don't mean much when you're using a polynomial.

# What is the effect of one additional unit of extraversion on the probability of volunteering when
# extraversion is 4? (Y = aX+bX^2 is a + 2bX)
# 0.0024

# Wald test, H0: joint nullity of extraversion and I(extraversion^2)
# stat = 11.8, p-value = 7.946e-6, on 2 and 1,418 DoF, VCOV: IID.
wald(m2, "extraversion")

# Which of these four graphs shows the distribution of the variable we'd be most likely to want to take
# the logarithm of? C.

etable(feols(log(extraversion) ~ male, data = Cowles))

# Interpretation: Men have extraversion scores 1.58 percent lower than women.

# What if we had instead run the regression feols(male ~ log(extraversion), data = Cowles)
# and got a coefficient of -.0158. How would we interpret this coefficient? (again, using the
# approximation).

# Interpretation: People with extraversion scores 1 percent higher are .0158 percentage points less likely to be a man.



# Use feols to run a regression of volunteered on male, extraversion, and their interaction.
m3 <- feols(volunteered ~ male * extraversion, data = Cowles)

etable(m3)

# What is the effect of a one-unit change in extraversion on the probability of a woman volunteering?


# Plug maleTRUE = 0 into the equation and see what the slope of extraversion is.
0.0157

# Plug maleTRUE = 1 into the equation and see what the slope of extraversion is.
0.0155

# What is the probability that a man with an extraversion score of 0 volunteers? Round to the fourth decimal place.
0.1947

