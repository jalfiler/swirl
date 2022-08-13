install.packages('swirl')

library(swirl)

install_course_github('NickCH-K','Econometrics')

swirl()

---------------------------------------------------------------
# 8: Experiments

# Load Libraries
library(tidyverse)
library(fixest)
library(vtable)


# You work for a retail clothing company that frequently
# offers coupons for clothes. The company is curious how
# effective those coupons actually are.
# 
# So they ask you to run an experiment!
# 
# You have a database of people who have bought from you
# before, and you are going to randomly send some of them
# coupons, and see how that affects the amount they buy the
# next time they shop with you (next_purchase).
# 
# Your design is going to be simple! Randomly assign each
# person to either be sent a coupon or not sent a coupon.


# Power Analysis: 50.50 randomized split
help("power.t.test")
power.t.test()


# You want to know the *minimum sample size* necessary to
# have 90% power to detect an effect at the 95% level 
# (alpha | = .05) with a two-sided test.
# 
# You look back on previous data for your company and, after
# some digging, find a few things:
# 
# 1. On average, people with coupons spend 10 more on each
# purchase than people without (this was not from randomized
# data, but it's a starting place for your anticipated effect).
# 
# 2. The standard deviation of purchase size is 40.
# 
# Use power.t.test() to run a power analysis with these
# parameters.
# 
# (Hint: you will need to fill in three arguments)
power.t.test(delta = 10, sd = 40, power = .9)
---------------------------------------
# Two-sample t test power calculation 

n = 337.2008
delta = 10
sd = 40
sig.level = 0.05
power = 0.9
alternative = two.sided

# NOTE: n is number in *each* group
---------------------------------------

# Rounding down to the nearest whole number, how many people
# do you need to recruit to your experiment?

# HINT: The "n" number shown in the power.t.test() 
# result is for EACH group, not BOTH groups.
337 * 2 = 674


----------
vtable(df)

# Use feols() to get the effect of being assigned_coupon on
# your next_purchase. Save the result as model1.
model1 <- feols(next_purchase ~ assigned_coupon, data = df)

etable(model1)

#  Use mutate() to create a new variable called attrit equal
#  to is.na(next_purchase), i.e. it's TRUE for anyone we
#  never see another purchase for (so it's missing, or NA).
df <- df %>% mutate(attrit = is.na(next_purchase))


# Now, check if anyone left the sample by checking how many
# times attrit is TRUE. You can do this by sum()ming up all
# the value of df$attrit
df %>% pull(attrit) %>% sum()

sumtable(df, group = "attrit", group.test = TRUE)


# Let's perform a regression using feols() of attrit on
# sent_coupon, last_purchase, and their interaction to see
# what's going on. Save the result as attritmodel
attritmodel <- feols(attrit ~ last_purchase*sent_coupon, data = df)

etable(attritmodel, digits = 3)

# Results:
#                                       attritmodel
# Dependent Var.:                            attrit
# 
# (Intercept)                      0.252*** (0.029)
# last_purchase                    -0.0004 (0.0005)
# sent_couponTRUE                   -0.084. (0.043)
# last_purchase x sent_couponTRUE -0.002** (0.0008)
# _______________________________ _________________
# S.E. type                                     IID
# Observations                                1,000
# R2                                        0.07866
# Adj. R2                                   0.07589
# ---
#   Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# Interpretation:
# Among those sent the coupon, those with lower last purchases 
# were more likely to attrit.

df_trim <- df %>% arrange(sent_coupon, last_purchase) %>% slice(-(1:107))

sumtable(df, group = "sent_coupon", group.test = TRUE)


# Regress next_purchase on assigned_coupon and gender, using lm.
model2 <- feols(next_purchase ~ assigned_coupon + gender, data = df)


etable(model1, model2)

etable(model1,model2, se = "hetero")

table(df$assigned_coupon, df$sent_coupon)


model3 <- feols(next_purchase ~ sent_coupon + gender, data = df)
help(feols)


model4 <- feols(next_purchase ~ gender | sent_coupon ~ assigned_coupon, se = 'hetero', data = df)

etable(model1, model2, model3, model4, se = 'hetero')
