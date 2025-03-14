################################################
#                                              #
#         5350 Thesis in Economics             #  
# Andres Cruz (25199) and Klara Holmer (25037) #
#                                              #
################################################

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

# ---- Design for Plots ---- #

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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

###################### ---- School Data English ---- ######################

#
#

# -- Data Preparation -- #

# Reading school data excel file and importing english sheet
school_data_english <- read_excel("school_data.xlsx", sheet = "English")

# Filter 
school_data_english <- school_data_english %>%
  filter(school_size != '1-49') %>%
  filter(academic_year != "2014/15", academic_year != "2015/16", academic_year != "2020/21", academic_year != "2021/22") 

# Variable Modification
school_data_english <- school_data_english %>%
  mutate(treatment_year = ifelse(academic_year == "2019/20", 1, 0), 
         treatment_group = ifelse(educational_stage == "gymnasieskola", 1, 0),
         DiD = treatment_year*treatment_group,
         private_ownership = ifelse(type_of_ownership == "Enskild", 1, 0),
         academic_year = as.factor(academic_year),
         school_municipality = as.factor(school_municipality),
         school_county = as.factor(school_municipality),
         academic_year_spring = as.numeric(paste0("20", sub(".*/", "", academic_year)))
  )

# Define the path to the output folder
path_output_english <- "/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/English/"

#
#

# -- Parallel Trend Assumption -- #

#
#

# - Parallel Trend Plot - #

# Time Trend
time_trend <- school_data_english %>% 
  drop_na() %>% 
  group_by(academic_year, educational_stage) %>% 
  summarise(average_share_students_F_eng = mean(share_students_F_eng))

# Plot the Time Trend for each educational stage
parallel_trend_plot <- ggplot(time_trend, 
                              mapping=aes(x=academic_year, y=average_share_students_F_eng, 
                                          group=educational_stage, color=educational_stage, shape=educational_stage)) + 
  # Adding dots
  geom_point(size = 2.5) +
  # Custom color and name 
  scale_color_manual(values = c('grundskola' = 'darkgrey', 'gymnasieskola' = 'black'),
                     labels = c('grundskola' = 'Lower Secondary, Grade 9', 'gymnasieskola' = 'Upper Secondary, Grade 10')) + 
  # Custom shapes
  scale_shape_manual(values = c('grundskola' = 20, 'gymnasieskola' = 18),  
                     labels = c('grundskola' = 'Lower Secondary, Grade 9', 'gymnasieskola' = 'Upper Secondary, Grade 10')) +
  # Connecting dots
  geom_line(size = 0.5) +
  # X intercept
  geom_vline(xintercept = 3.5, linetype = "dashed") + 
  # Modify text
  labs(
    title = "Parallel Trend Assumption - English",
    x = "Academic Year",
    y = "Average percent of students with F"
  ) +   
  # My theme
  my_theme

# Printing Plot
print(parallel_trend_plot)

# Use file.path() to ensure correct formatting of the file path
output_file_english <- file.path(path_output_english, "parallel_trend_plot.png")

# Save the plot
ggsave(output_file_english, parallel_trend_plot, bg = "transparent", width=5, height=4)

#
#
#









# - Dynamic Difference-in-Difference - #

# Set the reference (baseline) category to 2018/19 following Björkegren, Svaleryd and Vlachos (2024)
school_data_english$academic_year <- relevel(school_data_english$academic_year, ref="2018/19")

# Converting to to panel data
# school_data_panel <- pdata.frame(school_data_english, index = c("school_ID", "academic_year"))

# Check if the panel is balanced
# is.pbalanced(school_data_panel)

# Discrete Academic Year Variable
event_study_data <- school_data_english %>% 
  mutate(academic_year_spring = as.numeric(paste0("20", sub(".*/", "", academic_year)))) %>% 
  mutate(
    time_from_treatment = academic_year_spring - 2020, # MAGIC NUMBER CORRESPONDS TO THE TREATMENT YEAR
    lead_1 = case_when(time_from_treatment == -1 ~ 1, TRUE ~ 0),
    lead_2 = case_when(time_from_treatment == -2 ~ 1, TRUE ~ 0),
    lead_3 = case_when(time_from_treatment == -3 ~ 1, TRUE ~ 0),
    
    lag_0 = case_when(time_from_treatment == 0 ~ 1, TRUE ~ 0))


# Run Dynamic DiD Fixed Effects Model
lm_test <- lm(share_students_F_eng ~ (lead_3 + lead_2 + lead_1 + lag_0)*treatment_group, data = event_study_data)
summary(lm_test)

# Another test
test_2 <- lm(share_students_F_eng ~ 
               academic_year*treatment_group + share_foreign_background + share_postsecondary_parents + 
               share_active_certified_teachers + private_ownership + share_female_students, 
             data = event_study_data)

summary(test_2) # Storing the results

## with plm, slightly different
rob_cov_mat_plm <- plm::vcovHC(plm_elast_TWFE, type = "HC1", cluster="time")
robust_se_plm <- sqrt(diag(rob_cov_mat_plm))
robust_se_plm

































# Estimating the Dynamic DiD Model
dynamic_DiD <- lm(share_students_F_eng ~ academic_year*(treatment_group + share_foreign_background + share_postsecondary_parents + 
                                                          share_active_certified_teachers + private_ownership + share_female_students), 
                  data = school_data_english)

dynamic_DiD_summary <- summary(dynamic_DiD) # Storing the results

dynamic_DiD_clustered_se <- sqrt(diag(vcovCL(dynamic_DiD, cluster = ~ school_municipality))) # Clustered Standard Errors

# Order of Coefficients for the event study plot
plot_order <- c('academic_year2016/17:treatment_group', 
                'academic_year2017/18:treatment_group', 
                'academic_year2019/20:treatment_group',
                'academic_year2020/21:treatment_group')

# Extracting Coefficients and Using Clustered Standard Errors
dynamic_DiD_results_clustered <- tibble(
  estimates = c(dynamic_DiD_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_clustered_se[plot_order], 0),  # Use clustered SEs
  label = c(-3, -2, 0, 1, -1)
)

dynamic_did_plot <- ggplot(data = dynamic_DiD_results_clustered,  
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



#
#
#
#

# Use file.path() to ensure correct formatting of the file path
output_file_swedish <- file.path(path_output_english, "dynamic_did_plot.png")

# Save the plot
ggsave(output_file_swedish, dynamic_did_plot, bg = "transparent", width=5, height=4)

#
#

# -- Difference-in-Difference Estimation -- #

# Step 1 Estimate model with default standard errors
# Step 2 Construct clustered standard errors

# Model 1 - Default DiD Model
model_1 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD, 
              data = school_data_english)

summary(model_1)

clustered_se_1 <- sqrt(diag(vcovCL(model_1, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 2 - Adding share_postsecondary_parents
model_2 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD +
                share_postsecondary_parents, 
              data = school_data_english)

clustered_se_2 <- sqrt(diag(vcovCL(model_2, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 3 - Adding share_foreign_background
model_3 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background, 
              data = school_data_english)

clustered_se_3 <- sqrt(diag(vcovCL(model_3, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 4 - Adding share_active_certified_teachers
model_4 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers, 
              data = school_data_english)

clustered_se_4 <- sqrt(diag(vcovCL(model_4, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 5 - Adding type_of_principal
model_5 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers + 
                private_ownership, 
              data = school_data_english)

clustered_se_5 <- sqrt(diag(vcovCL(model_5, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 6 - Adding shares of female students
model_6 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers + 
                private_ownership + share_female_students, 
              data = school_data_english)

clustered_se_6 <- sqrt(diag(vcovCL(model_6, cluster = ~ school_municipality)))  # Clustered Standard Errors

#
#

# - Store Results - #

# List of models
models <- list(model_1, model_2, model_3, model_4, model_5, model_6)

# List of robust standard errors 
clustered_se_list <- list(clustered_se_1, clustered_se_2, clustered_se_3, clustered_se_4, clustered_se_5, clustered_se_6)

#
#

# - Exporting Regression Output - # 

# Use file.path() to ensure correct formatting of the file path
output_file_english <- file.path(path_output_english, "regression_output.html")

# Generate the Stargazer table dynamically
stargazer::stargazer(
  models,  # Use the list of models
  se = clustered_se_list,  # Use the list of clustered SEs
  model.numbers = FALSE,
  align = TRUE,
  dep.var.caption = "Dependent variable: Y",
  dep.var.labels = "Share of Students with F in English",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  covariate.labels = c("Treatment Year", "Treatment Group", "DiD",
                       "Share Postsecondary Parents", "Share Foreign Background",  "Share Active Certified Teachers", 
                       "Private Ownership", "Share of Female Students"),
  digits = 3,
  style = "aer",
  type = 'html',
  out = output_file_english  # Save the table as an HTML file
)


library(did)
period <- 2020

# estimate group-group time average treatment effects
did.att.gt <- att_gt(yname="share_students_F_eng",
                     tname="academic_year_spring",
                     idnam="school_ID",
                     gname="period",
                     data=school_data_english,
                     bstrap=FALSE,
                     cband=FALSE
                     )
summary(did.att.gt)

#> 



#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

###################### ---- School Data Mathematics ---- ######################

# -- Data Preparation -- #

# Reading school data excel file and importing english sheet
school_data_mathematics <- read_excel("school_data.xlsx", sheet = "Mathematics")

# Filter 
school_data_mathematics <- school_data_mathematics %>%
  filter(school_size != '1-49') %>%
  filter(academic_year != "2014/15", academic_year != "2015/16", academic_year != "2021/22") 

# Variable Modification
school_data_mathematics <- school_data_mathematics %>%
  mutate(treatment_year = ifelse(academic_year == "2019/20", 1, 0), 
         treatment_group = ifelse(educational_stage == "gymnasieskola", 1, 0),
         academic_year = as.factor(academic_year),
         DiD = treatment_year*treatment_group,
         private_ownership = ifelse(type_of_ownership == "Enskild", 1, 0)
  )

# Define the path to the output folder
path_output_mathematics <- "/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/Mathematics/"

#
#

# -- Parallel Trend Assumption -- #

#
#

# - Parallel Trend Plot - #

# Time Trend
time_trend <- school_data_mathematics %>% 
  drop_na() %>% 
  group_by(academic_year, educational_stage) %>% 
  summarise(average_share_students_F_ma = mean(share_students_F_ma))

# Plot the Time Trend for each educational stage
parallel_trend_plot <- ggplot(time_trend, 
                              mapping=aes(x=academic_year, y=average_share_students_F_ma, 
                                          group=educational_stage, color=educational_stage, shape=educational_stage)) + 
  # Adding dots
  geom_point(size = 2.5) +
  # Custom color and name 
  scale_color_manual(values = c('grundskola' = 'darkgrey', 'gymnasieskola' = 'black'),
                     labels = c('grundskola' = 'Lower Secondary, Grade 9', 'gymnasieskola' = 'Upper Secondary, Grade 10')) + 
  # Custom shapes
  scale_shape_manual(values = c('grundskola' = 20, 'gymnasieskola' = 18),  
                     labels = c('grundskola' = 'Lower Secondary, Grade 9', 'gymnasieskola' = 'Upper Secondary, Grade 10')) +
  # Connecting dots
  geom_line(size = 0.5) +
  # X intercept
  geom_vline(xintercept = 3.5, linetype = "dashed") + 
  # Modify text
  labs(
    title = "Parallel Trend Assumption - Mathematics",
    x = "Academic Year",
    y = "Average percent of students with F"
  ) +   
  # My theme
  my_theme

# Printing Plot
print(parallel_trend_plot)

# Use file.path() to ensure correct formatting of the file path
output_file_mathematics <- file.path(path_output_mathematics, "parallel_trend_plot.png")

# Save the plot
ggsave(output_file_mathematics, parallel_trend_plot, bg = "transparent", width=5, height=4)

#
#

# - Dynamic Difference-in-Difference - #

# Set the reference (baseline) category to 2018/19 following Björkegren, Svaleryd and Vlachos (2024)
school_data_mathematics$academic_year <- relevel(school_data_mathematics$academic_year, ref="2018/19")

# Estimating the Dynamic DiD Model
dynamic_DiD <- lm(share_students_F_ma ~ academic_year*(treatment_group + 
                                                         share_foreign_background + share_postsecondary_parents + 
                                                         share_active_certified_teachers + private_ownership + share_female_students), 
                  data = school_data_mathematics)

dynamic_DiD_summary <- summary(dynamic_DiD) # Storing the results

dynamic_DiD_clustered_se <- sqrt(diag(vcovCL(dynamic_DiD, cluster = ~ school_municipality))) # Clustered Standard Errors

# Order of Coefficients for the event study plot
plot_order <- c('academic_year2016/17:treatment_group', 
                'academic_year2017/18:treatment_group', 
                'academic_year2019/20:treatment_group',
                'academic_year2020/21:treatment_group')

# Extracting Coefficients and Using Clustered Standard Errors
dynamic_DiD_results_clustered <- tibble(
  estimates = c(dynamic_DiD_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_clustered_se[plot_order], 0),  # Use clustered SEs
  label = c(-3, -2, 0, 1, -1)
)

dynamic_did_plot <- ggplot(data = dynamic_DiD_results_clustered,  
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
output_file_mathematics <- file.path(path_output_mathematics, "dynamic_did_plot.png")

# Save the plot
ggsave(output_file_mathematics, dynamic_did_plot, bg = "transparent", width=5, height=4)


#
#

# -- Difference-in-Difference Estimation -- #

# Step 1 Estimate model with default standard errors
# Step 2 Construct clustered standard errors

# Model 1 - Default DiD Model
model_1 <- lm(share_students_F_ma ~ treatment_year + treatment_group + DiD, 
              data = school_data_mathematics)

clustered_se_1 <- sqrt(diag(vcovCL(model_1, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 2 - Adding share_postsecondary_parents
model_2 <- lm(share_students_F_ma ~ treatment_year + treatment_group + DiD +
                share_postsecondary_parents, 
              data = school_data_mathematics)

clustered_se_2 <- sqrt(diag(vcovCL(model_2, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 3 - Adding share_foreign_background
model_3 <- lm(share_students_F_ma ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background, 
              data = school_data_mathematics)

clustered_se_3 <- sqrt(diag(vcovCL(model_3, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 4 - Adding share_active_certified_teachers
model_4 <- lm(share_students_F_ma ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers, 
              data = school_data_mathematics)

clustered_se_4 <- sqrt(diag(vcovCL(model_4, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 5 - Adding type_of_principal
model_5 <- lm(share_students_F_ma ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers + 
                private_ownership, 
              data = school_data_mathematics)

clustered_se_5 <- sqrt(diag(vcovCL(model_5, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 6 - Adding shares of female students
model_6 <- lm(share_students_F_ma ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers + 
                private_ownership + share_female_students, 
              data = school_data_mathematics)

clustered_se_6 <- sqrt(diag(vcovCL(model_6, cluster = ~ school_municipality)))  # Clustered Standard Errors

#
#

# - Store Results - #

# List of models
models <- list(model_1, model_2, model_3, model_4, model_5, model_6)

# List of robust standard errors 
clustered_se_list <- list(clustered_se_1, clustered_se_2, clustered_se_3, clustered_se_4, clustered_se_5, clustered_se_6)

#
#

# - Exporting Regression Output - # 

# Use file.path() to ensure correct formatting of the file path
output_file_mathematics <- file.path(path_output_mathematics, "regression_output.html")

# Generate the Stargazer table dynamically
stargazer::stargazer(
  models,  # Use the list of models
  se = clustered_se_list,  # Use the list of clustered SEs
  model.numbers = FALSE,
  align = TRUE,
  dep.var.caption = "Dependent variable: Y",
  dep.var.labels = "Share of Students with F in Mathematics",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  covariate.labels = c("Treatment Year", "Treatment Group", "DiD",
                       "Share Postsecondary Parents", "Share Foreign Background",  "Share Active Certified Teachers", 
                       "Private Ownership", "Share of Female Students"),
  digits = 3,
  style = "aer",
  type = 'html',
  out = output_file_mathematics  # Save the table as an HTML file
)

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
###################### ---- School Data Swedish ---- ###########################

#
#

# -- Data Preparation -- #

# Reading school data excel file and importing english sheet
school_data_swedish <- read_excel("school_data.xlsx", sheet = "Swedish")

# Filter 
school_data_swedish <- school_data_swedish %>%
  filter(school_size != '1-49') %>%
  filter(academic_year != "2014/15", academic_year != "2015/16", academic_year != "2021/22") 

# Variable Modification
school_data_swedish <- school_data_swedish %>%
  mutate(treatment_year = ifelse(academic_year == "2019/20", 1, 0), 
         treatment_group = ifelse(educational_stage == "gymnasieskola", 1, 0),
         academic_year = as.factor(academic_year),
         DiD = treatment_year*treatment_group,
         private_ownership = ifelse(type_of_ownership == "Enskild", 1, 0)
         )

# Define the path to the output folder
path_output_swedish <- "/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/Swedish/"

#
#

# -- Parallel Trend Assumption -- #

#
#

# - Parallel Trend Plot - #

# Time Trend
time_trend <- school_data_swedish %>% 
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
  scale_color_manual(values = c('grundskola' = 'darkgrey', 'gymnasieskola' = 'black'),
                     labels = c('grundskola' = 'Lower Secondary, Grade 9', 'gymnasieskola' = 'Upper Secondary, Grade 10')) + 
  # Custom shapes
  scale_shape_manual(values = c('grundskola' = 20, 'gymnasieskola' = 18),  
                     labels = c('grundskola' = 'Lower Secondary, Grade 9', 'gymnasieskola' = 'Upper Secondary, Grade 10')) +
  # Connecting dots
  geom_line(size = 0.5) +
  # X intercept
  geom_vline(xintercept = 3.5, linetype = "dashed") + 
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
output_file_swedish <- file.path(path_output_swedish, "parallel_trend_plot.png")

# Save the plot
ggsave(output_file_swedish, parallel_trend_plot, bg = "transparent", width=5, height=4)

#
#

# - Dynamic Difference-in-Difference - #

# Set the reference (baseline) category to 2018/19 following Björkegren, Svaleryd and Vlachos (2024)
school_data_swedish$academic_year <- relevel(school_data_swedish$academic_year, ref="2018/19")

# Estimating the Dynamic DiD Model
dynamic_DiD <- lm(share_students_F_sv ~ academic_year*(treatment_group + 
                                                         share_foreign_background + share_postsecondary_parents + 
                                                         share_active_certified_teachers + private_ownership + share_female_students), 
                  data = school_data_swedish)

dynamic_DiD_summary <- summary(dynamic_DiD) # Storing the results

dynamic_DiD_clustered_se <- sqrt(diag(vcovCL(dynamic_DiD, cluster = ~ school_municipality))) # Clustered Standard Errors

# Order of Coefficients for the event study plot
plot_order <- c('academic_year2016/17:treatment_group', 
                'academic_year2017/18:treatment_group', 
                'academic_year2019/20:treatment_group',
                'academic_year2020/21:treatment_group')

# Extracting Coefficients and Using Clustered Standard Errors
dynamic_DiD_results_clustered <- tibble(
  estimates = c(dynamic_DiD_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_clustered_se[plot_order], 0),  # Use clustered SEs
  label = c(-3, -2, 0, 1, -1)
)

dynamic_did_plot <- ggplot(data = dynamic_DiD_results_clustered,  
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
output_file_swedish <- file.path(path_output_swedish, "dynamic_did_plot.png")

# Save the plot
ggsave(output_file_swedish, dynamic_did_plot, bg = "transparent", width=5, height=4)


#
#

# -- Difference-in-Difference Estimation -- #

# Step 1 Estimate model with default standard errors
# Step 2 Construct clustered standard errors

# Model 1 - Default DiD Model
model_1 <- lm(share_students_F_sv ~ treatment_year + treatment_group + DiD, 
              data = school_data_swedish)

clustered_se_1 <- sqrt(diag(vcovCL(model_1, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 2 - Adding share_postsecondary_parents
model_2 <- lm(share_students_F_sv ~ treatment_year + treatment_group + DiD +
                share_postsecondary_parents, 
              data = school_data_swedish)

clustered_se_2 <- sqrt(diag(vcovCL(model_2, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 3 - Adding share_foreign_background
model_3 <- lm(share_students_F_sv ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background, 
              data = school_data_swedish)

clustered_se_3 <- sqrt(diag(vcovCL(model_3, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 4 - Adding share_active_certified_teachers
model_4 <- lm(share_students_F_sv ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers, 
              data = school_data_swedish)

clustered_se_4 <- sqrt(diag(vcovCL(model_4, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 5 - Adding type_of_principal
model_5 <- lm(share_students_F_sv ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers + 
                private_ownership, 
              data = school_data_swedish)

clustered_se_5 <- sqrt(diag(vcovCL(model_5, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 6 - Adding shares of female students
model_6 <- lm(share_students_F_sv ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers + 
                private_ownership + share_female_students, 
              data = school_data_swedish)

clustered_se_6 <- sqrt(diag(vcovCL(model_6, cluster = ~ school_municipality)))  # Clustered Standard Errors

#
#

# - Store Results - #

# List of models
models <- list(model_1, model_2, model_3, model_4, model_5, model_6)

# List of robust standard errors 
clustered_se_list <- list(clustered_se_1, clustered_se_2, clustered_se_3, clustered_se_4, clustered_se_5, clustered_se_6)

#
#

# - Exporting Regression Output - # 

# Use file.path() to ensure correct formatting of the file path
output_file_swedish <- file.path(path_output_swedish, "regression_output.html")

# Generate the Stargazer table dynamically
stargazer::stargazer(
  models,  # Use the list of models
  se = clustered_se_list,  # Use the list of clustered SEs
  model.numbers = FALSE,
  align = TRUE,
  dep.var.caption = "Dependent variable: Y",
  dep.var.labels = "Share of Students with F in Swedish",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  covariate.labels = c("Treatment Year", "Treatment Group", "DiD",
                       "Share Postsecondary Parents", "Share Foreign Background",  "Share Active Certified Teachers", 
                       "Private Ownership", "Share of Female Students"),
  digits = 3,
  style = "aer",
  type = 'html',
  out = output_file_swedish  # Save the table as an HTML file
)
