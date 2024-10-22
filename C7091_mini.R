## C7091 Professional Skills for Data Science ####
## Miss S-J Childs
## Student ID: 24370500
## Mini-Project
## 2024-10-23

## CONTENTS ####
## 00 Setup
## 01 Explore data
## 02 Cleaning and manipulation
## 03 Describe and summarise
## 04 Assumption tests and Visualisation
## 05 ANOVA & Post hoc 1st hypothesis
## 06 ANOVA & Post hoc 2nd hypothesis
## 07 ANOVA & Post hoc 3rd hypothesis


## 00 Setup ####

# Load libraries
install.packages("tidyverse")
library(tidyverse) 
install.packages("readxl")
library(readxl)
install.packages("car")
library(car)
install.packages("Hmisc")
library(Hmisc)

# Set working directory
getwd()
setwd("C:/Users/sarah/Downloads")

# Read data
data <- read_excel("C:\\Users\\sarah\\Downloads\\C7091_data.xlsx") 


## 01 Explore data ####

str(data) # 4 variables, two numeric, two character for "Yield' and 'soil'

head(data) # appears to be in long format
tail(data)

attach(data)
unique(yr) # just two years 2016 and 2017
unique(soil) # two treatments and a control
unique(rep) # 1 to 5 reps



## 02 Cleaning and manipulation ####

# some variable names are not clear enough on their purpose

data <- data %>%
  rename(year = yr) %>% 
  rename(treatment = soil) %>% # more applicable name
  rename(yield = Yield) # personal preference against capitals

names(data)

# each variable needs to be categorised to the correct type

data$treatment <- as.factor(data$treatment) # needs to be factor with levels
class(treatment) # checking
levels(data$treatment) <- c("Reg_BNF", "Control", "Nano_BNF") # easier to understand

data$yield <- as.numeric(data$yield) # was character before
# warning message: NAs introduced by coercion
data$year <- as.factor(data$year)

glimpse(data)

# missing data check

data %>% 
  select(year, treatment, rep, yield) %>% 
  filter(!complete.cases(.)) # only one entry is NA, thus na.omit sensible

trial_data <- na.omit(data)

# Making sure they'll appear in a certain order

filtered_data <- trial_data %>% 
  filter(treatment %in% c("Control", "Reg_BNF", "Nano_BNF"))
filtered_data <- filtered_data %>%
  mutate(treatment = factor(treatment, 
                            levels = c("Control", "Reg_BNF", "Nano_BNF")))
trial_data <- filtered_data



## 03 Describe and summarise ####

# output from the dependent variable over 2016 and 2017
range(trial_data$yield) # quite a wide range
IQR(trial_data$yield)
mean(trial_data$yield)
median(trial_data$yield)
var(trial_data$yield)

# Split data by year for inspection
data_2016 <- trial_data %>% filter(year == 2016)
data_2017 <- trial_data %>% filter(year == 2017)

# Calculate for 2016
range(data_2016$yield) 
# although the min here is 0.0, we don't know if this is because of no 
# data or perhaps there was no yield. 
IQR(data_2016$yield)
mean(data_2016$yield)
median(data_2016$yield)
var(data_2016$yield)

# Calculate statistics for 2017
range(data_2017$yield)
IQR(data_2017$yield)
mean(data_2017$yield)
median(data_2017$yield)
var(data_2017$yield)

# Put all data into summary table

by_year_summary <- trial_data %>% 
  group_by(year) %>%
  reframe(
    range_yield = range(yield),
    IQR_yield = IQR(yield),
    mean_yield = mean(yield),
    median_yield = median(yield),
    var_yield = var(yield)
  )
print(by_year_summary)
# interesting differences across the years.

# Group data by treatment overall and summarise
treatment_summary <- trial_data %>%
  group_by(treatment) %>%
  reframe(
    range_yield = range(yield),
    IQR_yield = IQR(yield),
    mean_yield = mean(yield),
    median_yield = median(yield),
    var_yield = var(yield)
  )

print(treatment_summary)
# from initial observations it looks like the mean and median yields of 
# the Control and Reg_BNF are similar, but the Nano_BNF has higher yields



## 04 Assumption Tests  and visualisation ####

# Looking for Gaussian 

# Fitting the ANOVA model with trial_data
trial <- aov(yield ~ treatment, data = trial_data)


# Graph to examine Gaussian assumption of residuals

hist(rstandard(trial),
     main = "Gaussian?") # looks a bit skew

# Look at residuals with qqPlot()
qqPlot(x = trial,
       main = "Gaussian?") # hmm, a bit bendy


# fancy visual check ggplot
ggplot(trial_data, aes(x = yield)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "steelblue2", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(trial_data$yield),
                 sd = sd(trial_data$yield)), color = "orangered2") +
  labs(title = " Gaussian?", x = "Yield", y = "Density") 
# thank you Data Viz module

# Will try more formal tests from Bootcamp


## Shapiro-Wilk test 

shapiro_results <- shapiro.test(rstandard(trial))

print(shapiro_results)
# deviant W and very low P


## Homoscadasticity test

# Plot for homoscedasticity check
plot(formula = rstandard(trial) ~ fitted(trial),
     ylab = "trial: residuals",
     xlab = "trial: fitted values",
     main = "Spread similar across x?") 
abline(h = 0,
       lty = 2, lwd = 2, col = "orange") 

## Bartlett test

# Trying again for some Gaussian distribution...
bartlett.test(formula = yield ~ treatment, data = trial_data)
# low p-value again
# K-squared value indicates how much the data variances deviate from homogeneity
# The treatment groups don't have equal variances


## Graphing overall ANOVA

boxplot(yield ~ treatment, data = trial_data,
        ylab = "yield",
        xlab = "treatment",
        main = "Effect of treatment types on yield") 

abline(h = mean(trial_data$yield), # horizontal line for grand mean
       lty = 2, lwd = 2, col = "red") # helpful



## 05 First Null Hypothesis (h0) ANOVA & Post hoc ####

# h0: "The Nano_BNF will make no significant difference to yield compared to 
# the Control or Reg_BNF for both years 2016 and 2017."

H1_data <- trial_data %>%
  filter(treatment %in% c("Control", "Reg_BNF", "Nano_BNF"))

h1 <- aov(yield ~ treatment + year, data = H1_data) # fitted model

print(summary(h1))

# Treatment: F and p value highly significant, so treatment may affect yield
# Year: F and p value again significant, indicating possible influence from year
# High amount of residuals, quite some variation going on, due to non-normality?

# Visual check
ggplot(trial_data, aes(x = treatment, y = yield, color = as.factor(year))) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.2, 
               position = position_dodge(0.5)) +
  stat_summary(fun = mean, geom = "point", size = 3, 
               position = position_dodge(0.5)) +
  labs(title = "ANOVA Results: Yield by Treatment and Year",
       x = "Treatment", y = "Yield",
       color = "Year") +
  theme_minimal()
# Interesting! First thoughts would be that the Control would be similar in
# both years, but it is not. Perhaps due to being grown in the same plot? 

## Post hoc test

#TukeyHSD

tukey_results <- TukeyHSD(h1)
print(tukey_results)

# The Nano_BNF significantly increases yield in comparison to Control & Reg_BNF
# But the difference in yield between Reg_BNF and Control is not significant
# There is a significant difference in yields from 2016 to 2017

# Result: Null hypothesis rejection



## 06 Second Null Hypothesis (h0) ANOVA & Post hoc ####

# h0: "There will be no significant difference in the yield of Nano_BNF
# treatment from the year 2016 to the year 2017"

# Will need to first filter data required
Nano_BNF_data <- trial_data %>% 
  filter(treatment == "Nano_BNF")

h2 <- aov(yield ~ year, data = Nano_BNF_data) # fitted model

print(summary(h2))
# The yield does differ significantly between the years with the Nano_BNF
# How well does my model fit ..? The mean square of 716 looks high relative
# to the sum of squares (141688 for residuals) given 198 degrees of freedom.

# No post hoc required as there isn't more than two groups

# Visual check
ggplot(Nano_BNF_data, aes(x = year, y = yield, fill = year)) +
  geom_boxplot() +
  labs(title = "Yield for Nano_BNF Treatment by Year",
       x = "Year", y = "Yield") +
  theme_minimal()

# Result: Null hypothesis rejection



## 07 Third Null Hypothesis (h0) ANOVA & Post hoc ####

# h0: "There will be no significant difference between the yield of the 
# Reg_BNF and the yield of the Control in both years."

# Will need to filter data for Reg_BNF and Control
reg_control_data <- trial_data %>% 
  filter(treatment %in% c("Reg_BNF", "Control"))

h3 <- aov(yield ~ treatment * year, data = reg_control_data)

print(summary(h3))
# Treatment: Low F value and high p value suggest no significant difference
# Year: F value of 145.269 and p-value of <2e-16 do indicate a significant 
# difference in yields for 2016 and 2017. So the year appears to have more of 
# an effect on yield than the treatments.

# No post hoc required

# Visual check
ggplot(reg_control_data, aes(x = treatment, y = yield, 
      fill = as.factor(year))) +
  geom_boxplot() +
  labs(title = "Yield by Treatment and Year",
       x = "Treatment",
       y = "Yield",
       fill = "Year") +
  theme_minimal()

# Result: Null hypothesis is not rejected

