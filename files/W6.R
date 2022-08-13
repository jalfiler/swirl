install.packages('swirl')

library(swirl)

install_course_github('NickCH-K','Econometrics')

swirl()

---------------------------------------------------------------
# 6: Fixed Effects

# Install Packages
install.packages(c('tidyverse','vtable','wooldridge','fixest'),dependencies = TRUE)

# Load Libraries
library(tidyverse)
library(vtable)
library(wooldridge)
library(fixest)

# Now load the mathpnl data set from the **wooldridge** package with data(mathpnl).
data(mathpnl)
help(mathpnl)

# Select variables distid, year, math4, expp, lunch.
mathpnl <- mathpnl %>% select(distid, year, math4, expp, lunch)

# Use vtable() with the lush = TRUE option to look at the variables more
# closely, including the number of unique values each variable takes (nuniq).
vtable(lush = TRUE, data = mathpnl)

# Panel data is often described as "N by T". That is, the number of
# different individuals N and the number of time periods T.
N = numeric

# What is T in this data?
T = 7

# Add new variable (mutate) called num_years.
nyrs <- mathpnl %>% group_by(distid) %>% summarize(num_years = nuniq(year))

table(nyrs %>% pull(num_years))
7 
550 

# If T is 7, and num_years only ever takes the value 7 for every distid,
# then the data is balanced!

# Graph
# In which of those four graphs do the straight lines represent predicted
# values from the model math4 ~ expp with fixed effects for distid?
# Graph D


# Use feols() to regress math4 on expp, with lunch added as a control. Store
# the result as ols.
ols <- feols(math4 ~ expp, data = mathpnl)

# Run a regular feols() regression, with no fixed effects, of math4 on expp and lunch.
ols <- feols(math4 ~ expp + lunch, data = mathpnl)

# Now we will perform fixed effects "by hand" by de-meaning the data.
# We can use group_by(distid) to perform calculations within distid
# and x = x - mean(x) to subtract the individual mean from x.

# For example

# mathpnl_dm <- mathpnl %>% group_by(distid) %>% mutate(x_dm = x - mean(x),
# y_dm = y - mean(y))
# Would create mathpnl_dm with a new variable x_dm that is the de-meaned x
# and a new variable y_dm that is the de-meaned y.

# Create mathpnl_dm with de-meaned math4, expp, and lunch, called math4_dm,
# expp_dm, and lunch_dm, respectively.

mathpnl_dm <- 
  mathpnl %>% 
  group_by(distid) %>% 
  mutate(math4_dm = math4 - mean(math4), expp_dm = expp - mean(expp), lunch_dm = lunch - mean(lunch))


# (by the way, the next time you have to do the same calculation to a bunch
# of different variables in dplyr, try looking at the across() function to
# save yourself some time!).



# Use feols to regress math4_dm on expp_dm with lunch_dm as a control, and
# save the result as fe_dm
fe_dm <- feols(math4_dm ~ expp_dm + lunch_dm, data = mathpnl_dm)

etable(ols, fe_dm, digits = 3)



# What is the appropriate interpretation of the 0.012 in the table?
# A school with $1 more expenditure per student will have .012 percent more 4th graders 
# at acceptable than that same school in another year where expenditure is $1 lower.


# Use feols to regress math4 on expp, lunch, and factor(distid) and store the result as fe_lsdv.
fe_lsdv <- feols(math4 ~ expp + lunch + factor(distid), data = mathpnl)

# Use wald() from fixest to test if all the distid coefficients are jointly zero.
wald(fe_lsdv, "distid")

# Can we reject that the intercept is the same for all districts at the 1% level?
# YES

help(feols)

# Use feols() to run regress math4 on expp and lunch, with distid as fixed effects.
# Store the result as fe_feols.
fe_feols <- feols(math4 ~ expp + lunch | distid, data = mathpnl)


# Use etable to look at the results for ols, fe_dm, fe_lsdv, and fe_feols all together.
# Do etable() as normal, feeding in all four regression objects (in that order), and
# add the digits = 3 and keep = c('expp','expp_dm','lunch','lunch_dm') options.
etable(ols, fe_dm, fe_lsdv, fe_feols, digits = 3, keep = c('expp','expp_dm','lunch','lunch_dm'))


# Finally, use coefplot() with a list of models - list(ols, fe_lsdv, fe_feols), and the
# keep = c('expp','lunch') option, to plot out the different coefficients together.
coefplot(list(ols, fe_lsdv, fe_feols), keep = c('expp','lunch'))






