# ------ 5350 Thesis in Economics ------ #             
# ---- Andres Cruz (25199) and Klara Holmer (25037) ---- #
# -- Betyg -- #

#
#
#
#
#
#

# ------ Initializing ------ #

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

#
#
#
#
#
#

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

# ---- Data Preparation ---- #

# Reading school data excel file and importing english sheet
school_data <- read_excel("school_data_betyg.xlsx", sheet = "Betyg")

# Modifying DataFrame
school_data <- school_data %>%
  
  # Filter
  filter(school_size != '1-49') %>%
  
  # Variable Modification
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
    school_county = as.factor(school_county)
  )

# Converting to pandel data
school_data <- pdata.frame(school_data, index = c("school_ID", "academic_year"))

# Define the path to the output folder
path_output <- "/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/Betyg/"

#
#
#
#
#
#

# ------ Parallel Trend Assumption ------ #

# ---- Parallel Trend Plot ---- #

# Time Trend
time_trend <- school_data %>% 
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
  
  # Custom theme
  my_theme

# Print the plot
print(parallel_trend_plot)

# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output, "DiD_plot.png")

# Save the plot
ggsave(output_file, parallel_trend_plot, bg = "transparent", width=4, height=3, dpi = 1800)

#
#
#
#
#
#

# ---- Dynamic Difference-in-Difference ---- #

# Setting the reference year
school_data$academic_year <- relevel(school_data$academic_year, ref="18/19")

# -- Step 1 Modelling -- #

# Dynamic DiD Model WITHOUT controls
dynamic_DiD_without <- lm(average_grade_points ~ 
                            academic_year*treatment_group, 
                          data = school_data)
# Compute robust standard errors (HC1)
dynamic_DiD_without_rse <- sqrt(diag(vcovHC(dynamic_DiD_without, type = "HC1"))) 
# Storing the results
dynamic_DiD_without_summary <- summary(dynamic_DiD_without) 


# Dynamic DiD Model WITH controls
dynamic_DiD_with <- lm(
  average_grade_points ~ 
    academic_year*(treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students + private_ownership) + school_municipality, 
  data = school_data)
# Compute robust standard errors (HC1)
dynamic_DiD_with_rse <- sqrt(diag(vcovHC(dynamic_DiD_with, type = "HC1")))
# Store summary results
dynamic_DiD_with_summary <- summary(dynamic_DiD_with)

#
#
#
#
#
#

# -- Step 3 Creating Figure -- #

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
  standard_errors = c(dynamic_DiD_without_rse[plot_order], 0),
  label = c(-5, -4, -3, -2, 0, 1, 2, -1),
  model = "Without Controls"
)

# Extracting coefficients for the model WITH controls
dynamic_DiD_with_result <- tibble(
  estimates = c(dynamic_DiD_with_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_with_rse[plot_order], 0),
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
  ylab('Academic Year x Upper Secondary') +
  scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2)) +
  
  # Manually set colors and shapes for each model group
  scale_color_manual(values = c( "Without Controls" = "darkgrey", "With Controls" = "black")) +
  scale_shape_manual(values = c( "Without Controls" = 0, "With Controls" = 5)) +
  
  # Use your custom theme
  my_theme

# Print the combined plot
print(dynamic_did_plot)

# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output, "event_study_plot.png")

# Save the plot
ggsave(output_file, dynamic_did_plot, bg = "transparent", width=4, height=3, dpi = 1800)

#
#
#
#
#
#

# ---- Difference-in-Difference Estimation ---- #

# -- Step 1 Modelling -- # 

# Model 1 - Default DiD Model
model_1 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD, 
              data = school_data)
rse_1 <- sqrt(diag(vcovHC(model_1, type = "HC1"))) # Robust Standard Errors


# Model 2 - Adding share_foreign_background
model_2 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD +
                share_foreign_background, 
              data = school_data)
rse_2 <- sqrt(diag(vcovHC(model_2, type = "HC1"))) # Robust Standard Errors

# Model 3 - Adding share_active_certified_teachers
model_3 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD + 
                share_foreign_background + share_active_certified_teachers, 
              data = school_data)
rse_3 <- sqrt(diag(vcovHC(model_3, type = "HC1"))) # Robust Standard Errors

# Model 4 - Adding share_postsecondary_parents
model_4 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD + 
                share_foreign_background + share_active_certified_teachers + 
                share_postsecondary_parents, 
              data = school_data)
rse_4 <- sqrt(diag(vcovHC(model_4, type = "HC1"))) # Robust Standard Errors

# Model 5 - Adding share_female_students
model_5 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD + 
                share_foreign_background + share_active_certified_teachers + 
                share_postsecondary_parents + share_female_students, 
              data = school_data)
rse_5 <- sqrt(diag(vcovHC(model_5, type = "HC1"))) # Robust Standard Errors

# Model 6 - Adding private_ownership
model_6 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD + 
                share_foreign_background + share_active_certified_teachers + 
                share_postsecondary_parents + share_female_students + private_ownership, 
              data = school_data)
rse_6 <- sqrt(diag(vcovHC(model_6, type = "HC1"))) # Robust Standard Errors

# Model 7 - Adding Municipality Fixed Effect
model_7 <- lm(average_grade_points ~ treatment_year + treatment_group + DiD + 
                share_foreign_background + share_active_certified_teachers + 
                share_postsecondary_parents + share_female_students + private_ownership + school_municipality, 
              data = school_data)
rse_7 <- sqrt(diag(vcovHC(model_7, type = "HC1"))) # Robust Standard Errors

#
#
#
#
#
#

# -- Step 2 Creating a Table -- # 

# List of models
DiD_models <- list(model_1, model_2, model_3, model_4, model_5, model_6, model_7)

# List of robust standard errors 
DiD_rse <- list(rse_1, rse_2, rse_3, rse_4, rse_5, rse_6, rse_7)

# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output, "DiD_output.html")

# Generate the Stargazer table dynamically using the 'keep' argument
stargazer::stargazer(
  
  # List of Models
  DiD_models,       
  
  # List of Clustered Standard Errors
  se = DiD_rse,     
  
  # Table Formatting 
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "",
  dep.var.labels = "Average Grade Points",
  
  
  # Variables to keep 
  keep = "DiD|share_foreign_background|share_active_certified_teachers|share_postsecondary_parents|share_female_students|private_ownership",
  
  # Update covariate.labels to include only the variables you want to display
  covariate.labels = c("Upper Secondary x 2019/20", 
                       "Share Foreign Background",  
                       "Share Active Certified Teachers", 
                       "Share Postsecondary Parents", 
                       "Share of Female Students",
                       "Private Ownership"),
  # Three Digits
  digits = 3,
  
  # Omitting some statistics
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  
  # Adding Lines
  add.lines = list(c("School Controls", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), 
                   c("Municipality Fixed Effects", "No", "No", "No", "No", "No", "No", "Yes"),
                   c("Times Fixed Effects", "No", "No", "No", "No", "No", "No", "No")),
  
  
  # Changing the notes section
  notes.append = FALSE, # Exclude default significance levels text
  notes.label = "Note:", # Notes label
  notes = "HC1 standard errors. Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
  
  # Output settings
  type = 'html',
  out = output_file     # Save the table as an HTML file
)



