################################################
#                                              #
#         5350 Thesis in Economics             #  
# Andres Cruz (25199) and Klara Holmer (25037) #
#                                              #
################################################

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
library(sandwich)  
library(stargazer)


library(rio) 
library(vtable)
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


# Upper-secondary School, Grade 10 = Gymnasieskola Årskurs 1
# Lower-secondary School, Grade 9 = Grundskola Årskurs 9

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
  panel.grid.major.y = element_line(color = 'lightgrey', linetype ="solid"),
  
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

# ---- School Data Mathematics ---- #

#
#

# -- Data Preparation -- #

# Reading school data excel file and importing Mathematics sheet
school_data <- read_excel("school_data.xlsx", sheet = "Swedish")

# Filter 
school_data <- school_data %>%
  filter(graduating_students >= 0) %>%
  filter(academic_year != "2020/21", academic_year != "2021/22") 

# Variable Modification
school_data <- school_data %>%
  mutate(treatment_year = ifelse(academic_year == "2019/20", 1, 0), 
         treatment_group = ifelse(educational_stage == "upper secondary school", 1, 0),
         academic_year = as.factor(academic_year),
         DiD = treatment_year*treatment_group)  %>% 
  mutate(academic_year_spring = as.numeric(paste0("20", sub(".*/", "", academic_year))))

# Define the path to the output folder
path_output <- "/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/Swedish/"

#
#

# -- Parallel Trend Assumption -- #

#
#

# - Parallel Trend Plot - #

# Time Trend
time_trend <- school_data %>% 
  drop_na() %>% 
  group_by(academic_year, educational_stage) %>% 
  summarise(average_share_students_F_sv = mean(share_students_F_sv))

# Plot the Time Trend for each educational stage
parallel_trend_plot <- ggplot(time_trend, 
                              mapping=aes(x=academic_year, y=average_share_students_F_sv, 
                                          group=educational_stage, color=educational_stage, shape=educational_stage)) + 
  # Adding dots
  geom_point(size = 2.5) +
  # Custom color and name 
  scale_color_manual(values = c('lower secondary school' = 'darkgrey', 'upper secondary school' = 'black'),
                     labels = c('lower secondary school' = 'Lower Secondary School Grade 9', 'upper secondary school' = 'Upper Secondary School Grade 10')) + 
  # Custom shapes
  scale_shape_manual(values = c('lower secondary school' = 20, 'upper secondary school' = 18),  
                     labels = c('lower secondary school' = 'Lower Secondary School Grade 9', 'upper secondary school' = 'Upper Secondary School Grade 10')) +
  # Connecting dots
  geom_line(size = 0.5) +
  # X intercept
  geom_vline(xintercept = 5.5, linetype = "dashed") + 
  # Modify text
  labs(
    title = "Parallel Trend Assumption - Swedish",
    x = "Academic Year",
    y = "Average percent of students with F"
  ) +   
  # My theme
  my_theme

# Printing Plot
print(parallel_trend_plot)

# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output, "parallel_trend_plot.png")

# Save the plot
ggsave(output_file, parallel_trend_plot, bg = "transparent", width=5, height=4)

#
#

# - Event Study Plot - #

# Discrete Academic Year Variable
event_study_data <- school_data %>% 
  mutate(
    time_from_treatment = academic_year_spring - 2020, # MAGIC NUMBER CORRESPONDS TO THE TREATMENT YEAR
    lead_1 = case_when(time_from_treatment == -1 ~ 1, TRUE ~ 0),
    lead_2 = case_when(time_from_treatment == -2 ~ 1, TRUE ~ 0),
    lead_3 = case_when(time_from_treatment == -3 ~ 1, TRUE ~ 0),
    lead_4 = case_when(time_from_treatment == -4 ~ 1, TRUE ~ 0),
    lead_5 = case_when(time_from_treatment == -5 ~ 1, TRUE ~ 0),
    
    lag_0 = case_when(time_from_treatment == 0 ~ 1, TRUE ~ 0),
  ) %>% 
  mutate(academic_year_spring = as.factor(academic_year_spring))

# Set the reference (baseline) category to 2018/19 following Björkegren, Svaleryd and Vlachos (2024)
event_study_data$academic_year <- relevel(event_study_data$academic_year, ref="2018/19")

#
#

# - Dynamic Difference-in-Difference - #
dynamic_DiD <- lm(share_students_F_sv ~ academic_year + treatment_group + academic_year:treatment_group + 
                    share_foreign_background + share_postsecondary_parents + 
                    share_active_certified_teachers + type_of_principal,
                  data = event_study_data)
dynamic_DiD_summary <- summary(dynamic_DiD)

# Order of Coefficients for the event study plot
plot_order <- c('academic_year2014/15:treatment_group',
                'academic_year2015/16:treatment_group', 
                'academic_year2016/17:treatment_group', 
                'academic_year2017/18:treatment_group', 
                'academic_year2019/20:treatment_group')

# Extracting Coefficients
dynamic_DiD_results <- tibble(
  estimates = c(dynamic_DiD_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_summary$coefficients[plot_order, "Std. Error"], 0),
  label = c(-5, -4, -3, -2, 0, -1)
)

dynamic_did_plot <- ggplot(data = dynamic_DiD_results,  
                           aes(x = label, y = estimates)) +
  geom_errorbar(aes(ymin = estimates - 1.96 * standard_errors, 
                    ymax = estimates + 1.96 * standard_errors), 
                size = 0.5, width = 0.05, color = "black", alpha = 0.75) +  
  geom_point(shape = 18, size = 2.5, color = "black") + 
  xlab('Years before and after school closures') +
  ylab('Average percent of students with F') +
  geom_hline(yintercept = 0, linetype = "solid", color = 'brown', alpha = 0.75) +  
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  my_theme

# Print Dynamic DiD Plot
print(dynamic_did_plot)

# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output, "dynamic_did_plot.png")

# Save the plot
ggsave(output_file, dynamic_did_plot, bg = "transparent", width=5, height=4)

# - Leads and Lags Event Study - #

event_study <- lm(share_students_F_sv ~ (lead_5 + lead_4 + lead_3 + lead_2 + lag_0)*treatment_group +
                    share_foreign_background + share_postsecondary_parents + 
                    share_active_certified_teachers + type_of_principal, data = event_study_data)
event_study_summary <- summary(event_study)

# Order of Coefficients for the event study plot
plot_order <- c('lead_5:treatment_group',
                'lead_4:treatment_group', 
                'lead_3:treatment_group', 
                'lead_2:treatment_group', 
                'lag_0:treatment_group')

# Extracting Coefficients
event_study_results <- tibble(
  estimates = c(event_study_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(event_study_summary$coefficients[plot_order, "Std. Error"], 0),
  label = c(-5, -4, -3, -2, 0, -1)
)

lead_lag_plot <- ggplot(data = event_study_results,  
                        aes(x = label, y = estimates)) +
  geom_errorbar(aes(ymin = estimates - 1.96 * standard_errors, 
                    ymax = estimates + 1.96 * standard_errors), 
                size = 0.5, width = 0.05, color = "black", alpha = 0.75) +  
  geom_point(shape = 18, size = 2.5, color = "black") + 
  xlab('Years before and after school closures') +
  ylab('Average percent of students with F') +
  geom_hline(yintercept = 0, linetype = "solid", color = 'brown', alpha = 0.75) +  
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  my_theme

# Print Dynamic DiD Plot
print(lead_lag_plot)

#
#

# -- Difference-in-Difference Estimation -- #

# Step 1 Estimate model with default standard errors
# Step 2 Construct robust standard error

# Model 1 - Default DiD Model
model_1 <- lm(share_students_F_sv ~ treatment_year + treatment_group + DiD, 
              data = school_data)

robust_se_1 <- sqrt(diag(vcovHC(model_1, type = "HC1"))) # Robust Standard Errors

clustered_se_1 <- sqrt(diag(vcovCL(model_1, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 2 - Adding share_postsecondary_parents
model_2 <- lm(share_students_F_sv ~ treatment_year + treatment_group + DiD +
                share_foreign_background, 
              data = school_data)

robust_se_2 <- sqrt(diag(vcovHC(model_2, type = "HC1"))) # Robust Standard Errors
clustered_se_2 <- sqrt(diag(vcovCL(model_2, cluster = ~ school_municipality)))  # Clustered Standard Errors


# Model 3 - Adding share_postsecondary_parents
model_3 <- lm(share_students_F_sv ~ treatment_year + treatment_group + DiD +
                share_foreign_background + share_postsecondary_parents, 
              data = school_data)

robust_se_3 <- sqrt(diag(vcovHC(model_3, type = "HC1"))) # Robust Standard Errors
clustered_se_3 <- sqrt(diag(vcovCL(model_3, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 4 - Adding share_active_certified_teachers
model_4 <- lm(share_students_F_sv ~ treatment_year + treatment_group + DiD +
                share_foreign_background + share_postsecondary_parents + 
                share_active_certified_teachers, 
              data = school_data)

robust_se_4 <- sqrt(diag(vcovHC(model_4, type = "HC1"))) # Robust Standard Errors
clustered_se_4 <- sqrt(diag(vcovCL(model_4, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 5 - Adding type_of_principal
model_5 <- lm(share_students_F_sv ~ treatment_year + treatment_group + DiD +
                share_foreign_background + share_postsecondary_parents + 
                share_active_certified_teachers + type_of_principal, 
              data = school_data)

robust_se_5 <- sqrt(diag(vcovHC(model_5, type = "HC1"))) # Robust Standard Errors
clustered_se_5 <- sqrt(diag(vcovCL(model_5, cluster = ~ school_municipality)))  # Clustered Standard Errors

#
#

# - Store Results - #

# List of models
models <- list(model_1, model_2, model_3, model_4, model_5)

# List of robust standard errors
robust_se_list <- list(robust_se_1, robust_se_2, robust_se_3, robust_se_4, robust_se_5)

# List of robust standard errors 
clustered_se_list <- list(clustered_se_1, clustered_se_2, clustered_se_3, clustered_se_4, clustered_se_5)

#
#

# - Exporting Regression Output - # 

# Define the output file path
output_file <- file.path(path_output, "regression_output_default_standard_errors.html")

# Without Robust Standard Errors

# Generate the Stargazer table dynamically
stargazer::stargazer(
  models,  # Use the list of models
  model.numbers = FALSE,
  align = TRUE,
  dep.var.caption = "Dependent variable: Y",
  dep.var.labels = "Share of Students with F in Swedish",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  covariate.labels = c("Treatment Year", "Treatment Group", "DiD",
                       "Share Foreign Background", "Share Postsecondary Parents",
                       "Share Active Certified Teachers", "Type of Principal Kommunal"),
  digits = 3,
  style = "aer",
  type = 'html',
  out = output_file  # Save the table as an HTML file
)

# With Robust Standard Errors

# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output, "regression_output_robust_standard_errors.html")

# Generate the Stargazer table dynamically
stargazer::stargazer(
  models,  # Use the list of models
  se = robust_se_list,  # Use the list of robust SEs
  model.numbers = FALSE,
  align = TRUE,
  dep.var.caption = "Dependent variable: Y",
  dep.var.labels = "Share of Students with F in Swedish",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  covariate.labels = c("Treatment Year", "Treatment Group", "DiD",
                       "Share Foreign Background", "Share Postsecondary Parents",
                       "Share Active Certified Teachers", "Type of Principal Kommunal"),
  digits = 3,
  style = "aer",
  type = 'html',
  out = output_file  # Save the table as an HTML file
)

# With Clustered Standard Errors

# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output, "regression_output_clustered_standard_errors.html")

# Generate the Stargazer table dynamically
stargazer::stargazer(
  models,  # Use the list of models
  se = clustered_se_list,  # Use the list of clustered SEs
  model.numbers = FALSE,
  align = TRUE,
  dep.var.caption = "Dependent variable: Y",
  dep.var.labels = "Share of Students with F in Swedish",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  covariate.labels = c("Treatment Year", "Treatment Group", "DiD",
                       "Share Foreign Background", "Share Postsecondary Parents",
                       "Share Active Certified Teachers", "Type of Principal Kommunal"),
  digits = 3,
  style = "aer",
  type = 'html',
  out = output_file  # Save the table as an HTML file
)






