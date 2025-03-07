# 5350 Thesis in Economics
# Andres Cruz (25199) and Klara Holmer (25037)

#
#
#
#
#
#
#
#

# Initializing Working Space
# Clear workspace
rm(list=ls()) 

# Clear console
cat("\014") 

# Set working directory
setwd("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Processed Data")

# Loading packages 
library(tidyverse)
library(haven)
library(estimatr)
library(readxl) 
library(ggplot2)
library(dplyr)
library(plm) 
library(coefplot) 


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

# https://rpubs.com/phle/r_tutorial_difference_in_differences
# https://www.princeton.edu/~otorres/DID101R.pdf


# Upper-secondary, School Grade 10
# Lower-secondary, School Grade 9

#
#
#
#
#
#
#
#

# ---- Code for Plots ---- #

# Design Ideas From https://r-graph-gallery.com/web-scatterplot-and-ggrepel.html


# My Theme For Plots
my_theme <- theme(
  # Customize legend text, position, and background.
  legend.text = element_text(size = 9),
  legend.title = element_blank(),
  legend.position = "bottom",  # Move legend below the graph
  legend.direction = "horizontal",  # Arrange legend items side by side
  legend.background = element_rect(fill='transparent'),
  # This one removes the background behind each key in the legend
  legend.key = element_blank(),
  
  # Adjust axis parameters such as size and color.
  axis.text = element_text(size = 9, color = "black"),
  axis.title = element_text(size = 11, color = "black"),
  axis.ticks = element_line(colour = "black"),
  # Axis lines are now lighter than default
  axis.line = element_line(colour = "black"),
  
  # Only keep y-axis major grid lines, with a grey color and dashed type.
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = 'lightgrey', linetype ="dashed"),
  
  # Use a light color for the background of the plot and the panel.
  panel.background = element_rect(fill = 'transparent'),
  plot.background = element_rect(fill = 'transparent', color = NA)
)

#
#
#
#
#
#
#
#

# ---- School Data English ---- #

#
#

# -- Data Preparation -- #

# Reading school data excel file and importing engelska sheet
school_data_engelska <- read_excel("school_data.xlsx", sheet = "Engelska")

# Filtering and dummy variable creation 
school_data_engelska <- school_data_engelska %>%
  filter(läsår != "2020/21", läsår != "2021/22") %>%
  mutate(time = ifelse(läsår == "2019/20", 1, 0), 
         treated = ifelse(skolform == "gymnasieskola", 1, 0),
         läsår = as.factor(läsår),
         DiD = time*treated)

#
#

# -- Checking the Parallel Trend Assumption -- #

#  Time Trend
time_trend <- school_data_engelska %>% 
  drop_na() %>% 
  group_by(läsår, skolform) %>% 
  summarise(average = mean(andel_elever_F_eng))

# - Parallel Trend Plot - #
parallel_trend <- ggplot(time_trend, 
                         mapping=aes(x=läsår, y=average, group=skolform, color=skolform, shape=skolform)) + 
  # Adding dots
  geom_point(size = 1.25, alpha = 0.85) +
  # Custom color and name 
  scale_color_manual(values = c('grundskola' = 'darkgrey', 'gymnasieskola' = 'black'),
                     labels = c('grundskola' = 'School Grade 9', 'gymnasieskola' = 'School Grade 10')) + 
  # Custom shapes
  scale_shape_manual(values = c('grundskola' = 17, 'gymnasieskola' = 18),  
                     labels = c('grundskola' = 'School Grade 9', 'gymnasieskola' = 'School Grade 10')) +
  # Connecting dots
  geom_line() +
  # Modify text
  labs(
    title = "Parallel Trend Assumption - English",
    x = "Academic Year",
    y = "Average percent of students with F"
    ) +   
  # My theme
  my_theme

print(parallel_trend)
  
# Define the full file path
path_output_english <- "/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/English/parallel_trend_engelska.png"

# Save the plot
ggsave(path_output_english, parallel_trend, bg = "transparent", width=5, height=4)

#
#

# -- Statistical Modelling -- #

# -- Event Study -- # 

# Convert the DataFrame into a panel data structure
panel_engelska <- pdata.frame(school_data_engelska, index = c("skolkommun", "läsår"))

# Set the reference (baseline) category 
panel_engelska$läsår <- relevel(panel_engelska$läsår, ref="2018/19")


# Differnce-in-Differnce with Municipality Fixed Effects
model_1 <- plm(andel_elever_F_eng ~ läsår*treated, 
                data = panel_engelska, 
                model = "within", 
                effect = "individual")  

summary(model_1)

# obtain clustered standard errors
coeftest(model_1, vcov = function(x) 
  vcovHC(x, cluster = "group", type = "HC1"))




# Estimating 
model_lm_1 <- lm(andel_elever_F_eng ~ treated + time + did + skolkommun, data = school_data_engelska)
summary(model_lm_1)


# Generate the coefficient plot
coefplot <- coefplot(whisker_regression_1, predictor = c("treated:läsår")) +
  
  # Modify axis labels and title
  labs(
    title = "Whisker Plot",
    x = "Estimate",  # X-axis (previously Y-axis)
    y = "Academic Year"  # Y-axis (previously X-axis)
  ) +
  
  # Swap X and Y axes
  coord_flip() +  # Flips the axes
  
  # Customize X-axis with custom labels
  scale_y_discrete(labels = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20")) 

# Display the updated coefficient plot
print(coefplot)

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



#Check it is balanced
plm::is.pbalanced(panel_engelska$andel_elever_F_eng, panel_engelska$läsår)
# FALSE 


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


