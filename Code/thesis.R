# 5350 Thesis in Economics
# Andres Cruz (25199) and Klara Holmer (25037)

# Initializing Working Space
# Clear workspace
rm(list=ls()) 

# Clear console
cat("\014") 

# Set working directory
setwd("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/thesis_code")

# Loading packages 
library(tidyverse)
library(haven)
library(estimatr)
library(readxl) 
library(ggplot2)
library(dplyr)
library(plm) 

library(rio) 
library(vtable)
library(sandwich)
library(stargazer)
library(car)
library(magrittr) 
library(ggthemes) 
library(rmarkdown) 
library(knitr) 
library(kableExtra) 
library(tinytex) 
library(broom) 
library(lmtest) 
library(mosaic) 
library(AER) 
library(coefplot) 

# https://rpubs.com/phle/r_tutorial_difference_in_differences
# https://www.princeton.edu/~otorres/DID101R.pdf

# ---- School Data English ---- #

# Reading school data excel file and importing engelska sheet
school_data_engelska <- read_excel("school_data.xlsx", sheet = "Engelska")

# Filtering and dummy variable creation 
school_data_engelska <- school_data_engelska %>%
  filter(läsår != "2020/21", läsår != "2021/22") %>%
  mutate(time = ifelse(läsår == "2019/20", 1, 0), 
         treated = ifelse(skolform == "gymnasieskola", 1, 0))

# -- Whisker Plot - #
whisker_regression_1 <-lm_robust(andel_elever_F_eng ~ treated*läsår, data = school_data_engelska)
model_summary <- summary(whisker_regression_1)
model_summary

# Create the custom year vector
läsår <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20")

# Extracting Coefficients for the Estimate
coefs_of_interest <- c("treated:läsår2015/16", 
                       "treated:läsår2016/17",
                       "treated:läsår2017/18",
                       "treated:läsår2018/19",
                       "treated:läsår2019/20")
estimates <- model_summary$coefficients[coefs_of_interest, "Estimate"]

# Extracting Coefficients for the Std. Error
coefs_of_interest <- c("treated:läsår2015/16", 
                       "treated:läsår2016/17",
                       "treated:läsår2017/18",
                       "treated:läsår2018/19",
                       "treated:läsår2019/20")
std_errors <- model_summary$coefficients[coefs_of_interest, "Std. Error"]

# Creating a table
DiD_plot <- tibble(
  mean = estimates,  # coefficient estimates
  sd   = std_errors,  # standard errors
  year = läsår) # Year

DiD_plot %>% 
  ggplot(aes(x = year, y = mean)) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean - sd*1.96, ymax = mean + sd*1.96), width = 0.2,
                position = position_dodge(0.05)) +
  labs(title="Whisker plot for coefficients of DD estimates - English",
       x ="Läsår", 
       y = "Coefficients")


################ I AM FIGURING THIS PART OUT

# Convert school_data_engelska into a panel data frame
panel_engelska <- pdata.frame(school_data_engelska, "skol_enhetskod")

#Check it is balanced
plm::is.pbalanced(panel_engelska$andel_elever_F_eng, panel_engelska$läsår)
# FALSE 

# - Model Estimation - #
# Within model
regression_1 <- plm(andel_elever_F_eng ~ time + treated + time:treated, 
                    data = panel_engelska, index=c("skolkommun"), model = "within")

# obtain clustered standard errors
coeftest(regression_1, vcov = function(x) 
  vcovHC(x, cluster = "group", type = "HC1"))

#
#
#
#
#
#

# --- Matemathics ----
school_data_matematik <- read_excel("school_data.xlsx", sheet = "Matematik")

# Filtering and dummy variable creation 
school_data_matematik <- school_data_matematik %>%
  filter(läsår != "2020/21", läsår != "2021/22") %>%
  mutate(time = ifelse(läsår == "2019/20", 1, 0), 
         treated = ifelse(skolform == "gymnasieskola", 1, 0))

# -- Whisker Plot - #
whisker_regression_2 <-lm_robust(andel_elever_F_ma ~ treated*läsår, data = school_data_matematik)
model_summary <- summary(whisker_regression_2)
model_summary

# Create the custom year vector
läsår <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20")

# Extracting Coefficients for the Estimate
coefs_of_interest <- c("treated:läsår2015/16", 
                       "treated:läsår2016/17",
                       "treated:läsår2017/18",
                       "treated:läsår2018/19",
                       "treated:läsår2019/20")
estimates <- model_summary$coefficients[coefs_of_interest, "Estimate"]

# Extracting Coefficients for the Std. Error
coefs_of_interest <- c("treated:läsår2015/16", 
                       "treated:läsår2016/17",
                       "treated:läsår2017/18",
                       "treated:läsår2018/19",
                       "treated:läsår2019/20")
std_errors <- model_summary$coefficients[coefs_of_interest, "Std. Error"]

# Creating a table
DiD_plot <- tibble(
  mean = estimates,  # coefficient estimates
  sd   = std_errors,  # standard errors
  year = läsår) # Year

DiD_plot %>% 
  ggplot(aes(x = year, y = mean)) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean - sd*1.96, ymax = mean + sd*1.96), width = 0.2,
                position = position_dodge(0.05)) +
  labs(title="Whisker plot for coefficients of DD estimates - Matemathics",
       x ="Läsår", 
       y = "Coefficients")


#
#
#
#
#
#

# --- Swedish ----
school_data_svenska <- read_excel("school_data.xlsx", sheet = "Svenska")

# Filtering and dummy variable creation 
school_data_svenska <- school_data_svenska %>%
  filter(läsår != "2020/21", läsår != "2021/22") %>%
  mutate(time = ifelse(läsår == "2019/20", 1, 0), 
         treated = ifelse(skolform == "gymnasieskola", 1, 0))

# -- Whisker Plot - #
whisker_regression_3 <-lm_robust(andel_elever_F_sv ~ treated*läsår, data = school_data_svenska)
model_summary <- summary(whisker_regression_3)
model_summary

# Create the custom year vector
läsår <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20")

# Extracting Coefficients for the Estimate
coefs_of_interest <- c("treated:läsår2015/16", 
                       "treated:läsår2016/17",
                       "treated:läsår2017/18",
                       "treated:läsår2018/19",
                       "treated:läsår2019/20")
estimates <- model_summary$coefficients[coefs_of_interest, "Estimate"]

# Extracting Coefficients for the Std. Error
coefs_of_interest <- c("treated:läsår2015/16", 
                       "treated:läsår2016/17",
                       "treated:läsår2017/18",
                       "treated:läsår2018/19",
                       "treated:läsår2019/20")
std_errors <- model_summary$coefficients[coefs_of_interest, "Std. Error"]

# Creating a table
DiD_plot <- tibble(
  mean = estimates,  # coefficient estimates
  sd   = std_errors,  # standard errors
  year = läsår) # Year

DiD_plot %>% 
  ggplot(aes(x = year, y = mean)) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean - sd*1.96, ymax = mean + sd*1.96), width = 0.2,
                position = position_dodge(0.05)) +
  labs(title="Whisker plot for coefficients of DD estimates - Svenska",
       x ="Läsår", 
       y = "Coefficients")


