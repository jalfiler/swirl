install.packages('swirl')

library(swirl)

install_course_github('NickCH-K','Econometrics')

swirl()

---------------------------------------------------------------
# 7: Differences-in-differences

# Suppose that you were a seller of bread yeast. 
# You are interested in how consumer interest in sourdough bread
# baking changed during the 2020 COVID-19 lockdowns.

# Install Packages
install.packages('gtrendsR')

# Load Libraries
library(tidyverse)
library(lubridate)
library(gtrendsR)
library(fixest)
library(haven)
library(vtable)


# Google Trends allows us to see the volume of searches for the term "sourdough bread" before and after the
# lockdowns started in the United States. Why might comparing these volumes before and after be a misleading
# estimate of the effects of lockdowns on sourdough bread interest?

# 1: People might Google less as the weather got nicer after lockdowns.
# 2: Bread consumption might be seasonal
# 3: Something else that promoted bread might have happened at the same time as lockdowns.

# Instead, we will attempt to compare the search volume for sourdough with an unrelated product, like "cereal."
# While imperfect, at least we will have a baseline volume of searches to compare against.
trends <- gtrends(c("sourdough bread", "cereal"), time = "2020-01-01 2020-08-01", geo = "US")


# Trends is a list holding multiple dataframes. We are interested in the interest_over_time dataframe.
trends_time <- trends$interest_over_time %>% as_tibble() %>% mutate(date = ymd(date))

# Examine the structure of trends_time using vtable.
vtable::vtable(trends_time)


1.
# A simple difference-in-differences model takes the form:

# outcome = treated + post_treatment + treated:post_treatment, where : indicates an interaction
# between two variables.

# To estimate this model, first we need to create these variables.

# First, add a logical dummy variable to trends_time named 'treated' set to TRUE when keyword =
# "sourdough bread" and FALSE otherwise.
trends_time <- trends_time %>% mutate(treated = keyword == "sourdough bread")


2. 
# Next, add a logical dummy variable to trends_time named post_treatment equal to TRUE when date is
# greater than or equal to 2020-03-18, the first state lockdown order.

# Note - you should use as.Date("2020-03-18") so that R knows you are working with a date value.
trends_time <- trends_time %>% mutate(post_treatment = date >= 2020-03-18)


3.
# Using feols() and your two new dummy variables, estimate the simple difference-in-difference
# regression model, using hits (a measure of how popular a search term is) as your outcome variable.
 
# Save the resulting estimates as bread_diff_in_diff.
bread_diff_in_diff <- feols(hits ~ treated * post_treatment, data = trends_time)

# Summarize output using etable().
etable(bread_diff_in_diff)

# Interpretation of etable: 
# That is a large increase in search volume in the post-period relative to searches for cereal.

# Graph:
ggplot(trends_time, aes(x=date, y=hits, color = keyword)) + geom_line() + geom_vline(xintercept = as.Date("2020-03-18"))

# Interpretation:
# After following similar trends for months, there is a sharp uptick in sourdough searches near the 
# time period when the lockdowns begin!

# That said, the uptick was almost completely gone by July. Your difference-in-difference estimate
# picked up the average increase in search volume over the whole post-period.


---------------
# Next, we will do a similar exercise to study the relationship between 
# labor supply and the Earned Income Tax Credit (EITC).

# The EITC is a tax credit aimed at the working poor. The tax credit is designed to be zero if you
# have no income and to increase as you earn more.

# The EITC is meant to reward work. It is an empirical question if it actually increases labor supply.

# Here, we will study the effects of the 1994 EITC expansion, which dramatically increased the size of the award.

eitc <-read_dta("https://github.com/CausalReinforcer/Stata/raw/master/eitc.dta") 
# data on a sample of women and their labor supply decisions

1.
# Summarize the eitc tibble using vtable/
vtable::vtable(eitc)

2. 
# Create a logical dummy variable in the eitc dataframe named post_expansion equal to 1 for years
# equal to or greater than the year the EITC expanded (1994)
eitc <- eitc %>% mutate(post_expansion = year >= 1994)

3.
# Create a logical dummy variable in the eitc dataframe named treated_women equal to 1 for women with any children
eitc <- eitc %>% mutate(treated_women = 1)

4. 
# What assumption is required to claim that the difference-in-difference estimator in this case would
# estimate a causal effect of the policy on labor supply?

#Answer:
# Labor supply for women with and without children would have 
# followed the same trend after 1994 in the absence of the expansion.


# Calculate the mean labor supply by 'treated_women' and 'post_expansion' status using the following command.
eitc %>% group_by(treated_women, post_expansion) %>% summarise(mean_ls = mean(work))


# Using these means, calculate by hand the difference-in-difference estimated increase in working
# probability for women with children after the 1994 EITC expansion
(0.491-0.446) - (0.573-0.575)
[1] 0.047


1. 
# Now estimate the simple difference-in-difference regression model.

# Save the resulting estimates as eitc_diff_in_diff
eitc_diff_in_diff <- feols(work ~ treated_women * post_expansion, data = eitc)

# Summarize your output using etable
etable(eitc_diff_in_diff)


# What is the interpretation of the coefficient on treated_women:post_expansion?

# Interpretation:
# Work hours grew for women with kids 5 percentage points more 
# than it did for those without kids from before to after the expansion.

# Use feols() to regress work on the treated*after term you had before (treated_women*post_expansion,
# note the non-interaction-term parts will be dropped, but you still want * rather than : since
# otherwise it does some weird stuff), and then use to add fixed effects for group (children, the
# different numbers of children women have which determine their treatment assignment) and time
# period (year). Put the group before the time periods so the standard errors will be clustered at
# the group level. Save the result as twfe.
twfe <- feols(work ~ treated_women*post_expansion | children + year, data = eitc)

# Put both eitc_diff_in_diff and twfe in the same etable.
etable(eitc_diff_in_diff,twfe)

fixef(twfe)

