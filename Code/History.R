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

# ------ Design for Plots ------ #

# My Theme For Plots
my_theme <- theme(
  
  # Customize legend text, position, and background.
  legend.text = element_text(size = 7),
  legend.title = element_blank(),
  legend.position = "bottom",  # Move legend below the graph
  legend.direction = "horizontal",  # Arrange legend items side by side
  legend.background = element_rect(fill='transparent'),
  legend.key = element_blank(),  # This one removes the background behind each key in the legend

  # Adjust axis parameters such as size and color.
  axis.text = element_text(size = 7, color = "black"),
  axis.title = element_text(size = 7, color = "black"),
  axis.ticks = element_line(colour = "black", size = 0.25),
  axis.title.y = element_text(vjust = +3),
  axis.title.x = element_text(vjust = -3),
  
  # Axis lines are now lighter than default
  axis.line = element_line(colour = "black", size = 0.25),

  # Only keep y-axis major grid lines, with a grey color and dashed type.
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = 'lightgrey', linetype ="solid", size = 0.25),
  
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

###################### ------ School Data Betyg ------ ######################

#
#
#
#
#

# ---- Data Preparation ---- #

# Reading school data excel file and importing english sheet
school_data_betyg <- read_excel("school_data_betyg.xlsx", sheet = "Betyg")

# Filter 
school_data_betyg <- school_data_betyg %>%
  filter(school_size != '1-49') 

# Variable Modification
school_data_betyg <- school_data_betyg %>%
  mutate(
    
    # Difference in Difference Variables
    treatment_year = ifelse(academic_year == "2019/20", 1, 0), 
    treatment_group = ifelse(educational_stage == "gymnasieskola", 1, 0),
    DiD = treatment_year*treatment_group,
    
    # Control Variables
    private_ownership = ifelse(type_of_ownership == "Enskild", 1, 0),
    academic_year = paste0(substring(academic_year, 3, 4),substring(academic_year, 5, 7)),
    academic_year_spring = as.numeric(paste0(sub(".*/", "", academic_year))), 
    academic_year = as.factor(academic_year), 

    # Fixed Effects
    school_ID = as.factor(school_ID),
    school_municipality = as.factor(school_municipality),
    school_county = as.factor(school_municipality)
  )

# Converting to pandel data
school_data_betyg <- pdata.frame(school_data_betyg, index = c("school_ID", "academic_year"))

# Define the path to the output folder
path_output_betyg <- "/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/Betyg/"

#
#
#
#
#

# ---- Parallel Trend Assumption ---- #

#
#
#
#

# -- Parallel Trend Plot -- #

# Time Trend
time_trend <- school_data_betyg %>% 
  drop_na() %>% 
  group_by(academic_year_spring, educational_stage) %>% 
  summarise(average_grade_points = mean(average_grade_points))

# Plot the Time Trend for each educational stage
parallel_trend_plot <- ggplot(time_trend, 
                              mapping=aes(x=academic_year_spring, y=average_grade_points, 
                                          group=educational_stage, color=educational_stage, shape=educational_stage)) + 
  
  # Vertical reference line
  geom_vline(xintercept = 19.5, linetype = "solid", color = "brown", size = 0.25) +
  
  # Data lines and points
  geom_line(size = 0.25) +
  geom_point(size = 1) +
  
  # Custom color and shape assignments
  scale_color_manual(values = c("grundskola" = "darkgrey", "gymnasieskola" = "black"),
                     labels = c("grundskola" = "Lower Secondary, Grade 9", 
                                "gymnasieskola" = "Upper Secondary, Grade 10-12")) + 
  scale_shape_manual(values = c("grundskola" = 21, "gymnasieskola" = 21),
                     labels = c("grundskola" = "Lower Secondary, Grade 9", 
                                "gymnasieskola" = "Upper Secondary, Grade 10-12")) +
  
  # Custom x-axis with manually specified breaks and labels
  scale_x_continuous(breaks = c(15, 16, 17, 18, 19, 20, 21, 22),
                     labels = c("14/15", "15/16", "16/17", "17/18", "18/19", "19/20", "20/21", "21/22")) +
  
  # Axis labels and y-axis limits
  xlab("Academic Year") +
  ylab("Average Grade Points") +
  ylim(13, 15) +
  
  # Custom theme
  my_theme

# Print the plot
print(parallel_trend_plot)

# Use file.path() to ensure correct formatting of the file path
output_file_betyg <- file.path(path_output_betyg, "parallel_trend_plot.png")

# Save the plot
ggsave(output_file_betyg, parallel_trend_plot, bg = "transparent", width=4, height=3, dpi = 1800)

#
#
#
#

# -- Dynamic Difference-in-Difference -- #

#
#

# Setting the reference year
school_data_betyg$academic_year <- relevel(school_data_betyg$academic_year, ref="18/19")

#
#

# - Step 1 Modelling - #

# Dynamic DiD Model WITHOUT controls or Model 1
dynamic_DiD_without <- lm(average_grade_points ~ academic_year*treatment_group, data = school_data_betyg)
dynamic_DiD_without_summary <- summary(dynamic_DiD_without) # Storing the results
dynamic_DID_without_cse <- sqrt(diag(vcovCL(dynamic_DiD_without, cluster = ~ school_municipality))) # Clustered Standard Errors

# Model 2
dynamic_DiD_2 <- lm(average_grade_points ~ academic_year*treatment_group + 
                      academic_year_spring*(share_foreign_background),
                    data = school_data_betyg)
dynamic_DiD_2_cse <- sqrt(diag(vcovCL(dynamic_DiD_2, cluster = ~ school_municipality))) # Clustered Standard Errors

# Model 3
dynamic_DiD_3 <- lm(average_grade_points ~ academic_year*treatment_group + 
                      academic_year_spring*(share_foreign_background + share_active_certified_teachers),
                    data = school_data_betyg)
dynamic_DiD_3_cse <- sqrt(diag(vcovCL(dynamic_DiD_3, cluster = ~ school_municipality))) # Clustered Standard Errors


# Model 4
dynamic_DiD_4 <- lm(average_grade_points ~ academic_year*treatment_group + 
                      academic_year_spring*(share_foreign_background + share_active_certified_teachers) + 
                      share_postsecondary_parents,
                    data = school_data_betyg)
dynamic_DID_4_cse <- sqrt(diag(vcovCL(dynamic_DiD_4, cluster = ~ school_municipality))) # Clustered Standard Errors

# Model 5
dynamic_DiD_5 <- lm(average_grade_points ~ academic_year*treatment_group + 
                      academic_year_spring*(share_foreign_background + share_active_certified_teachers) + 
                      share_postsecondary_parents + private_ownership,
                    data = school_data_betyg)
dynamic_DiD_5_cse <- sqrt(diag(vcovCL(dynamic_DiD_5, cluster = ~ school_municipality))) # Clustered Standard Errors

# Model 6
dynamic_DiD_6 <- lm(average_grade_points ~ academic_year*treatment_group + 
                      academic_year_spring*(share_foreign_background + share_active_certified_teachers) + 
                      share_postsecondary_parents + private_ownership + share_female_students,
                    data = school_data_betyg)
dynamic_DID_6_cse <- sqrt(diag(vcovCL(dynamic_DiD_6, cluster = ~ school_municipality))) # Clustered Standard Errors

# Dynamic DiD Model WITH controls, fixed effects, and time trend, (Model 7)
dynamic_DiD_with <- lm(
  average_grade_points ~ 
    academic_year*treatment_group + academic_year_spring*(share_foreign_background + share_active_certified_teachers) +
    share_postsecondary_parents + private_ownership + share_female_students + school_ID, 
  data = school_data_betyg)
# Storing the results
dynamic_DiD_with_summary <- summary(dynamic_DiD_with) 
# Clustered Standard Errors
dynamic_DiD_with_cse <- sqrt(diag(vcovCL(dynamic_DiD_with, cluster = ~ school_municipality)))  

#
#

# - Step 2 Creating Figure - #

#
#

# Order of Coefficients for the event study plot
plot_order <- c('academic_year14/15:treatment_group', 
                'academic_year15/16:treatment_group', 
                'academic_year16/17:treatment_group', 
                'academic_year17/18:treatment_group', 
                'academic_year19/20:treatment_group',
                'academic_year20/21:treatment_group',
                'academic_year21/22:treatment_group') 

# Extracting coefficients for the model WITHOUT controls
dynamic_DiD_without_result <- tibble(
  estimates = c(dynamic_DiD_without_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DID_without_cse[plot_order], 0),
  label = c(-5, -4, -3, -2, 0, 1, 2, -1),
  model = "Without Controls"
)

# Extracting coefficients for the model WITH controls
dynamic_DiD_with_result <- tibble(
  estimates = c(dynamic_DiD_with_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_with_cse[plot_order], 0),
  label = c(-5, -4, -3, -2, 0, 1, 2, -1),
  model = "With Controls"
)

# Combine the results into one data frame
combined_results <- bind_rows(dynamic_DiD_without_result, dynamic_DiD_with_result)

# Reorder the factor levels so "Without Controls" comes first
combined_results <- combined_results %>%
  mutate(model = factor(model, levels = c("Without Controls", "With Controls")))

# Create the event study plot with overlaid results
dynamic_did_plot <- ggplot(data = combined_results, aes(x = label, y = estimates, color = model, shape = model, group = model)) +
  
  # Bottom Layer: Horizontal reference line
  geom_hline(yintercept = 0, linetype = "solid", color = 'brown', size = 0.25) +
  
  # Middle Layer: Error bars (dodged to avoid complete overlap)
  geom_errorbar(aes(ymin = estimates - 1.96 * standard_errors, 
                    ymax = estimates + 1.96 * standard_errors), 
                size = 0.25, width = 0.05, position = position_dodge(width = 0.2)) +
  
  # Top Layer: Points (also dodged)
  geom_point(size = 1, position = position_dodge(width = 0.2)) +
  
  # Axis labels and limits
  xlab('Years before and after spring 2020 school closures') +
  ylab('Academic Year x Share of Students with F') +
  ylim(-0.5, 0.5) +
  scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2)) +
  
  # Manually set colors and shapes for each model group
  scale_color_manual(values = c( "Without Controls" = "darkgrey", "With Controls" = "black")) +
  scale_shape_manual(values = c( "Without Controls" = 0, "With Controls" = 5)) +
  
  # Use your custom theme
  my_theme

# Print the combined plot
print(dynamic_did_plot)

# Use file.path() to ensure correct formatting of the file path
output_file_betyg <- file.path(path_output_betyg, "dynamic_did_plot.png")

# Save the plot
ggsave(output_file_betyg, dynamic_did_plot, bg = "transparent", width=4, height=3, dpi = 1800)

#
#
#
#

# - Step 3 Creating a table - #

# - Store Results - #

# List of models
models <- list(dynamic_DiD_without, 
               dynamic_DiD_2, 
               dynamic_DiD_3, 
               dynamic_DiD_4, 
               dynamic_DiD_5, 
               dynamic_DiD_6,
               dynamic_DiD_with)

# List of robust standard errors 
clustered_se <- list(dynamic_DiD_without_cse, 
                     dynamic_DiD_2_cse,  
                     dynamic_DiD_3_cse, 
                     dynamic_DiD_4_cse, 
                     dynamic_DiD_5_cse, 
                     dynamic_DiD_6_cse,
                     dynamic_DiD_with_cse)

#
#
#
#

# Use file.path() to ensure correct formatting of the file path
output_file_betyg <- file.path(path_output_betyg, 
                               "Event_Study_output.html")


# Generate the Stargazer table dynamically using the 'keep' argument
stargazer::stargazer(
  
  # List of Models
  models,
  
  # List of Clustered Standard Erros
  se = clustered_se,      
  
  # Table Formatting 
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "",
  dep.var.labels = "Average Grade Points",
  
  
  # Variables to keep
  keep = 
  "academic_year14/15:treatment_group|
  academic_year15/16:treatment_group|
  academic_year16/17:treatment_group|
  academic_year19/20:treatment_group|
  academic_year19/20:treatment_group|
  academic_year20/21:treatment_group|
  academic_year21/22:treatment_group",
  
  # Renaming Variables
  covariate.labels = c("Upper Secondary x 2014/2015",
                       "Upper Secondary x 2015/16",
                       "Upper Secondary x 2016/17",
                       "Upper Secondary x 2017/18",
                       "Upper Secondary x 2018/19",
                       "Upper Secondary x 2019/20",
                       "Upper Secondary x 2020/21",
                       "Upper Secondary x 2021/22"),
  digits = 3,
 
  # Omitting some statistics
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  
  # Adding Lines
  add.lines = list(c("School Controls", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), 
                   c("School Effects", "No", "No", "No", "No", "No", "No", "Yes")),
  
  # Changing the notes section
  notes.append = FALSE, # Exclude default significance levels text
  notes.label = "Note:", # Notes label
  notes = "Standard errors are clustered at the municipality level. Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
  
  # Output settings
  type = 'html',
  out = output_file_betyg      # Save the table as an HTML file
)

#
#
#
#
#
#
#

# -- Difference-in-Difference Estimation -- #

# Step 1 Estimate model with default standard errors
# Step 2 Construct clustered standard errors

# Model 1 - Default DiD Model
model_1 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD, 
              data = school_data_betyg)

clustered_se_1 <- sqrt(diag(vcovCL(model_1, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 2 - Adding share_postsecondary_parents
model_2 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD +
                share_postsecondary_parents, 
              data = school_data_betyg)

clustered_se_2 <- sqrt(diag(vcovCL(model_2, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 3 - Adding share_foreign_background
model_3 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background, 
              data = school_data_betyg)

clustered_se_3 <- sqrt(diag(vcovCL(model_3, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 4 - Adding share_active_certified_teachers
model_4 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers, 
              data = school_data_betyg)

clustered_se_4 <- sqrt(diag(vcovCL(model_4, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 5 - Adding type_of_principal
model_5 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers + 
                private_ownership, 
              data = school_data_betyg)

clustered_se_5 <- sqrt(diag(vcovCL(model_5, cluster = ~ school_municipality)))  # Clustered Standard Errors

# Model 6 - Adding shares of female students
model_6 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD + 
                share_postsecondary_parents + share_foreign_background + share_active_certified_teachers + 
                private_ownership + share_female_students, 
              data = school_data_betyg)

clustered_se_6 <- sqrt(diag(vcovCL(model_6, cluster = ~ school_municipality)))  # Clustered Standard Errors

# - Store Results - #

# List of models
models <- list(model_1, model_2, model_3, model_4, model_5, model_6)

# List of robust standard errors 
clustered_se_list <- list(clustered_se_1, clustered_se_2, clustered_se_3, clustered_se_4, clustered_se_5, clustered_se_6)

#
#

# - Exporting Regression Output - #

# Use file.path() to ensure correct formatting of the file path
output_file_betyg <- file.path(path_output_betyg, "DiD_output.html")

# Generate the Stargazer table dynamically using the 'keep' argument
stargazer::stargazer(
  models,                      # Use the list of models
  se = clustered_se_list,      # Use the list of clustered SEs
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "",
  dep.var.labels = "Average Grade Points",
  # Update covariate.labels to include only the variables you want to display
  covariate.labels = c("Upper Secondary x 2019/20", 
                       "Share Postsecondary Parents", 
                       "Share Foreign Background",  
                       "Share Active Certified Teachers", 
                       "Private Ownership", 
                       "Share of Female Students"),
  digits = 3,
  keep = "DiD|share_postsecondary_parents|share_foreign_background|share_active_certified_teachers|private_ownership|share_female_students",
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  add.lines = list(c("Fixed Effects", "No", "No", "No", "No", "No", "No")),
  add.lines = list(c("Controls", "No", "Yes", "Yes", "Yes", "Yes", "Yes")),
  
  
  # Changing the notes section
  notes.append = FALSE, # Exclude default significance levels text
  notes.label = "Note:", # Notes label
  notes = "Standard errors are clustered at the municipality level. Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
  
  # Output settings
  type = 'html',
  out = output_file_betyg      # Save the table as an HTML file
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
school_data_english <- read_excel("school_data_ämnen.xlsx", sheet = "English")

# Filter 
school_data_english <- school_data_english %>%
  filter(school_size != '1-49') 

# Variable Modification
school_data_english <- school_data_english %>%
  mutate(
    
    # Difference in Difference Variables
    treatment_year = ifelse(academic_year == "2019/20", 1, 0), 
    treatment_group = ifelse(educational_stage == "gymnasieskola", 1, 0),
    DiD = treatment_year*treatment_group,
    
    # Control Variables
    private_ownership = ifelse(type_of_ownership == "Enskild", 1, 0),
    academic_year = as.factor(academic_year), 
    academic_year_spring = as.numeric(paste0("20", sub(".*/", "", academic_year))), 
    
    
    # Fixed Effects
    school_ID = as.factor(school_ID),
    school_municipality = as.factor(school_municipality),
    school_county = as.factor(school_municipality)
  )

# Converting to pandel data
school_data_english <- pdata.frame(school_data_english, index = c("school_ID", "academic_year"))


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
  geom_vline(xintercept = 5.5, linetype = "dashed") + 
  # Modify text
  labs(
    x = "Academic Year",
    y = "Average percent of students with F"
  ) +   
  ylim(0, 12) +
  # My theme
  my_theme

# Printing Plot
print(parallel_trend_plot)

# Use file.path() to ensure correct formatting of the file path
output_file_english <- file.path(path_output_english, "parallel_trend_plot.png")

# Save the plot
ggsave(output_file_english, parallel_trend_plot, bg = "transparent", width=6, height=3)

#
#
#
#

# - Dynamic Difference-in-Difference - #

# Setting the reference year
school_data_english$academic_year <- relevel(school_data_english$academic_year, ref="2018/19")

# Estimating the Dynamic DiD Model
dynamic_DiD <- lm(
  share_students_F_eng ~ 
    academic_year*(treatment_group + share_foreign_background + share_postsecondary_parents + 
                     share_active_certified_teachers + private_ownership + share_female_students), 
  data = school_data_english)

# Storing the results
dynamic_DiD_summary <- summary(dynamic_DiD) 

# Clustered Standard Errors
dynamic_DID_clustered_se <- sqrt(diag(vcovCL(dynamic_DiD, cluster = ~ school_municipality)))  

# Order of Coefficients for the event study plot
plot_order <- c('academic_year2014/15:treatment_group', 
                'academic_year2015/16:treatment_group', 
                'academic_year2016/17:treatment_group', 
                'academic_year2017/18:treatment_group', 
                'academic_year2019/20:treatment_group',
                'academic_year2020/21:treatment_group',
                'academic_year2021/22:treatment_group')

# Extracting Coefficients and Using Clustered Standard Errors
dynamic_did_plot_table <- tibble(
  estimates = c(dynamic_DiD_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DID_clustered_se[plot_order], 0),  # Use clustered SEs
  label = c(-5, -4, -3, -2, 0, 1, 2, -1)
)

dynamic_did_plot <- ggplot(data = dynamic_did_plot_table,  
                           aes(x = label, y = estimates)) +
  geom_errorbar(aes(ymin = estimates - 1.96 * standard_errors, 
                    ymax = estimates + 1.96 * standard_errors), 
                size = 0.5, width = 0.05, color = "black", alpha = 0.75) +  
  geom_point(shape = 18, size = 2.5, color = "black") + 
  xlab('Years before and after school closures') +
  ylab('Share of Student with F in English') +
  geom_hline(yintercept = 0, linetype = "solid", color = 'brown', alpha = 0.75) +  
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  ylim(-3, 3) + 
  my_theme

# Print Dynamic DiD Plot
print(dynamic_did_plot)

# Use file.path() to ensure correct formatting of the file path
output_file_english <- file.path(path_output_english, "dynamic_did_plot.png")

# Save the plot
ggsave(output_file_english, dynamic_did_plot, bg = "transparent", width=6, height=3)

#
#

# -- Difference-in-Difference Estimation -- #

# Step 1 Estimate model with default standard errors
# Step 2 Construct clustered standard errors

# Model 1 - Default DiD Model
model_1 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD, 
              data = school_data_english)

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

summary(model_6)

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
  # Model estimates and standard errors
  models,                      # Use the list of models
  se = clustered_se_list,      # Use the list of clustered SEs
  
  # Top row modification 
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "",
  dep.var.labels = "Share of Students with F in English",
  
  # Update covariate.labels to include only the variables you want to display
  omit = "treatment_year|treatment_group",  # Omit the first two variables by name
  covariate.labels = c("Upper Secondary x 2019/20", 
                       "Share Postsecondary Parents", 
                       "Share Foreign Background",  
                       "Share Active Certified Teachers", 
                       "Private Ownership", 
                       "Share of Female Students"),
  digits = 3,
  
  # Additional changes
  add.lines = list(c("School Fixed Effects", "No", "No", "No", "No", "No", "No")),
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  
  # Changing the notes section
  notes.append = FALSE, # Exclude default significance levels text
  notes.label = "Note:", # Notes label
  notes = "This table presents our DiD model estimates. Standard errors are clustered at the municipality level. Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
  
  # Output
  type = 'html',
  out = output_file_english      # Save the table as an HTML file
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

###################### ---- School Data Mathematics ---- ######################

# -- Data Preparation -- #

# Reading school data excel file and importing english sheet
school_data_mathematics <- read_excel("school_data_ämnen.xlsx", sheet = "Mathematics")

# Filter 
school_data_mathematics <- school_data_mathematics %>%
  filter(school_size != '1-49')
# Variable Modification
school_data_mathematics <- school_data_mathematics %>%
  mutate(
    
    # Difference in Difference Variables
    treatment_year = ifelse(academic_year == "2019/20", 1, 0), 
    treatment_group = ifelse(educational_stage == "gymnasieskola", 1, 0),
    DiD = treatment_year*treatment_group,
    
    # Control Variables
    private_ownership = ifelse(type_of_ownership == "Enskild", 1, 0),
    academic_year = as.factor(academic_year), 
    academic_year_spring = as.numeric(paste0("20", sub(".*/", "", academic_year))), 
    
    
    # Fixed Effects
    school_ID = as.factor(school_ID),
    school_municipality = as.factor(school_municipality),
    school_county = as.factor(school_municipality)
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
  geom_vline(xintercept = 5.5, linetype = "dashed") + 
  # Modify text
  labs(
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
plot_order <- c('academic_year_factor2014/15:treatment_group', 
                'academic_year_factor2015/16:treatment_group', 
                'academic_year_factor2016/17:treatment_group', 
                'academic_year_factor2017/18:treatment_group', 
                'academic_year_factor2019/20:treatment_group',
                'academic_year_factor2020/21:treatment_group',
                'academic_year_factor2021/22:treatment_group')

# Extracting Coefficients and Using Clustered Standard Errors
dynamic_did_plot_table <- tibble(
  estimates = c(dynamic_DiD_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DID_clustered_se[plot_order], 0),  # Use clustered SEs
  label = c(-5, -4, -3, -2, 0, 1, 2, -1)
)

dynamic_did_plot <- ggplot(data = dynamic_DiD_results_clustered,  
                           aes(x = label, y = estimates)) +
  geom_errorbar(aes(ymin = estimates - 1.96 * standard_errors, 
                    ymax = estimates + 1.96 * standard_errors), 
                size = 0.5, width = 0.05, color = "black", alpha = 0.75) +  
  geom_point(shape = 18, size = 2.5, color = "black") + 
  xlab('Years before and after school closures') +
  ylab('Average percent of students with F Mathematics') +
  geom_hline(yintercept = 0, linetype = "solid", color = 'brown', alpha = 0.75) +  
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  ylim(-1,1) + 
  my_theme

# Print Dynamic DiD Plot
print(dynamic_did_plot)

# Use file.path() to ensure correct formatting of the file path
output_file_mathematics <- file.path(path_output_mathematics, "dynamic_did_plot.png")

# Save the plot
ggsave(output_file_mathematics, dynamic_did_plot, bg = "transparent", width=6, height=3)


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

summary(model_6)
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
  # Model estimates and standard errors
  models,                      # Use the list of models
  se = clustered_se_list,      # Use the list of clustered SEs
  
  # Top row modification 
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "",
  dep.var.labels = "Share of Students with F in Mathemathics",
  
  # Update covariate.labels to include only the variables you want to display
  omit = "treatment_year|treatment_group",  # Omit the first two variables by name
  covariate.labels = c("Upper Secondary x 2019/20", 
                       "Share Postsecondary Parents", 
                       "Share Foreign Background",  
                       "Share Active Certified Teachers", 
                       "Private Ownership", 
                       "Share of Female Students"),
  digits = 3,
  
  # Additional changes
  add.lines = list(c("School Fixed Effects", "No", "No", "No", "No", "No", "No")),
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  
  # Changing the notes section
  notes.append = FALSE, # Exclude default significance levels text
  notes.label = "Note:", # Notes label
  notes = "This table presents our DiD model estimates. Standard errors are clustered at the municipality level. Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
  
  # Output
  type = 'html',
  out = output_file_mathematics      # Save the table as an HTML file
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
school_data_swedish <- read_excel("school_data_ämnen.xlsx", sheet = "Swedish")

# Filter 
school_data_swedish <- school_data_swedish %>%
  filter(school_size != '1-49') %>%
  filter(academic_year != "2014/15", academic_year != "2015/16", academic_year != "2020/21", academic_year != "2021/22") 

# Variable Modification
school_data_swedish <- school_data_swedish %>%
  mutate(treatment_year = ifelse(academic_year == "2019/20", 1, 0), 
         treatment_group = ifelse(educational_stage == "gymnasieskola", 1, 0),
         DiD = treatment_year*treatment_group,
         private_ownership = ifelse(type_of_ownership == "Enskild", 1, 0),
         school_ID = as.factor(school_ID),
         academic_year = as.factor(academic_year),
         school_municipality = as.factor(school_municipality),
         school_county = as.factor(school_municipality),
         academic_year_spring = as.numeric(paste0("20", sub(".*/", "", academic_year)))
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
  ylim(0,6) +
  my_theme

# Printing Plot
print(parallel_trend_plot)

# Use file.path() to ensure correct formatting of the file path
output_file_swedish <- file.path(path_output_swedish, "parallel_trend_plot.png")

# Save the plot
ggsave(output_file_swedish, parallel_trend_plot, bg = "transparent", width=6, height=3)

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
                'academic_year2019/20:treatment_group')

# Extracting Coefficients and Using Clustered Standard Errors
dynamic_DiD_results_clustered <- tibble(
  estimates = c(dynamic_DiD_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_clustered_se[plot_order], 0),  # Use clustered SEs
  label = c(-3, -2, 0, -1)
)

dynamic_did_plot <- ggplot(data = dynamic_DiD_results_clustered,  
                           aes(x = label, y = estimates)) +
  geom_errorbar(aes(ymin = estimates - 1.96 * standard_errors, 
                    ymax = estimates + 1.96 * standard_errors), 
                size = 0.5, width = 0.05, color = "black", alpha = 0.75) +  
  geom_point(shape = 18, size = 2.5, color = "black") + 
  xlab('Years before and after school closures') +
  ylab('Average percent of students with F Swedish') +
  geom_hline(yintercept = 0, linetype = "solid", color = 'brown', alpha = 0.75) +  
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  ylim(-2,2) +
  my_theme

# Print Dynamic DiD Plot
print(dynamic_did_plot)

# Use file.path() to ensure correct formatting of the file path
output_file_swedish <- file.path(path_output_swedish, "dynamic_did_plot.png")

# Save the plot
ggsave(output_file_swedish, dynamic_did_plot, bg = "transparent", width=6, height=3)


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
  # Model estimates and standard errors
  models,                      # Use the list of models
  se = clustered_se_list,      # Use the list of clustered SEs
  
  # Top row modification 
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "",
  dep.var.labels = "Share of Students with F in Swedish",
  
  # Update covariate.labels to include only the variables you want to display
  omit = "treatment_year|treatment_group",  # Omit the first two variables by name
  covariate.labels = c("Upper Secondary x 2019/20", 
                       "Share Postsecondary Parents", 
                       "Share Foreign Background",  
                       "Share Active Certified Teachers", 
                       "Private Ownership", 
                       "Share of Female Students"),
  digits = 3,
  
  # Additional changes
  add.lines = list(c("School Fixed Effects", "No", "No", "No", "No", "No", "No")),
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  
  # Changing the notes section
  notes.append = FALSE, # Exclude default significance levels text
  notes.label = "Note:", # Notes label
  notes = "This table presents our DiD model estimates. Standard errors are clustered at the municipality level. Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
  
  # Output
  type = 'html',
  out = output_file_swedish      # Save the table as an HTML file
)

