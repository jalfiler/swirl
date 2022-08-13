install.packages('swirl')

library(swirl)

install_course_github('NickCH-K','Econometrics')

swirl()

---------------------------------------------------------------
# 8: Instrumental Variables

# This lesson will focus on instrumental variables and how to perform IV in R.

# Install Packages
install.packages('fixest', 'AER', 'tidyverse')

# Load Libraries
library(tidyverse)
library(fixest)
library(AER)

# Use data() to load CigarettesSW
data(CigarettesSW)


# The data set has data on each state in both 1985 and 1995.
# To simplify things, let's look only at 1995.
#  
# Use FILTER to keep only the observations for which year is
# 1995. Overwrite the original CigarettesSW with this.
CigarettesSW <- CigarettesSW %>% filter(year == 1995)

# We are interested in the effect of cigarette prices on the
# number of cigarette packs purchased.
# 
# First, we need to adjust prices for inflation.
# 
# Use mutate to create a new variable in CigarettesSW called
# rprice that's equal to price/cpi.
CigarettesSW <- CigarettesSW %>% mutate(rprice = price/cpi)

# Now let's get a raw association between packs and prices
# 
# Use feols() to regress the logarithm (log()) of packs on
# the logarithm of inflation-adjusted prices and store the
# result as ols.
ols <- feols(log(packs) ~ log(rprice), data = CigarettesSW)

etable(ols)

# How can we interpret the -1.21 coefficient?
# Interpretation: 
# A 1% increase in rprice is associated with a 1.21% decrease in packs. 


# Why might we be skeptical of thinking the result in ols
# represents a causal effect?

# HINT: 
# Endogeneity arises from things that might affect both treatment and outcome.

# Interpretation: 
# Because the price of cigarettes is responsive to how many packs people want to buy


# Use mutate to create a new variable called taxdiff, equal
# to (taxs-tax)/cpi.
CigarettesSW <- CigarettesSW %>% mutate(taxdiff = (taxs-tax)/cpi)

help(feols)

# Use feols to regress log(packs) on log(rprice), using
# taxdiff as an instrument. No controls included. To skip
# controls, just put a 1 where the controls should be.

# Use feols with two parts to the formula- log(packs)
# regressed on 1 in the first, and log(rprice) regressed on
# taxdiff in the second.

# Save the result as iv1.
iv1 <- feols(log(packs) ~ 1 | log(rprice) ~ taxdiff, data = CigarettesSW)

etable(ols, iv1)

# Assuming that our instrument is relevant and valid, 
# how can we best interpret the -1.08 coefficient?

# Interpretation:
# A 1% increase in rprice driven by tax increases will decrease packs sold by 1.08%
  
# Even if we assume the instrument is relevant and valid,
# why might we still be concerned that this estimate is biased?
# Answer: IV is biased in small samples, and this sample is very small


# Get the "first stage" model from the iv1 object using
# summary(iv1, stage = 1), showing the impact of the
# instrument on the endogenous variable. (note iv1$iv_first_stage would also work)
summary(iv1, stage = 1)

# Does this result raise any clear
# concerns about taxdiff being a weak instrument? NO.

# Rerun your feols model again but also include tax as an
# instrument. Save the result as iv2.
iv2 <- feols(log(packs) ~ 1 | log(rprice) ~ taxdiff + tax, data = CigarettesSW)

fitstat(iv2, type = 'ivf')

help(fitstat)

fitstat(iv2, 'sargan')

# What is the best interpretation to take from the null
# result in the overidentification test?
# If one of the instruments is valid, they both are.


# Finally, use coefplot() to plot ols, iv1, and iv2.
# Remember, coefplot doesn't just take the model names
# itself, it takes a list() of model names.
coefplot(list(ols,iv1,iv2))












