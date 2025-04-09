# ------ 5350 Thesis in Economics ------ #             
# ---- Andres Cruz (25199) and Klara Holmer (25037) ---- #
# -- English -- #

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
library(tidyr)
library(knitr)
library(kableExtra)

# Define the path to the output folder
path_output <- "/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/English/"

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

# Reading school data excel file and importing sheet
school_data <- read_excel("school_data_ämnen.xlsx", sheet = "English")

# Modifying DataFrame
school_data <- school_data %>%
  
  # Renaming Dependet Variable name
  rename(share_students_F = share_students_F_eng) %>%
  
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

#
#
#
#
#
#

# ------ Summary Statistics ------ #

# - Ownership - #

# Prepare the summary table (same as before)
ownership_table <- school_data %>%
  group_by(academic_year, private_ownership) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(
    names_from = private_ownership,
    values_from = Count,
    names_prefix = "Private_ownership_"
  ) %>%
  arrange(`academic_year`)

# Transpose the table
ownership_table_t <- ownership_table %>%
  pivot_longer(
    cols = c(`Private_ownership_0`, `Private_ownership_1`),
    names_to = "Ownership Type",
    values_to = "Count"
  ) %>%
  pivot_wider(
    names_from = `academic_year`,
    values_from = Count
  )

# Create LaTeX table
ownership_table_t %>%
  kbl(format = "latex", booktabs = TRUE,
      caption = "Number of Schools by Ownership Type and Academic Year") %>%
  kable_styling(latex_options = c("hold_position"))

#
#
#
#

# - Educational Stage - #

# Step 1: Summarize by academic_year and educational_stage
stage_table <- school_data %>%
  group_by(academic_year, educational_stage) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(
    names_from = academic_year,
    values_from = Count
  )

# Step 2: Generate LaTeX table
stage_table %>%
  kbl(format = "latex", booktabs = TRUE,
      caption = "Number of Schools by Educational Stage and Academic Year") %>%
  kable_styling(latex_options = c("hold_position"))

#
#
#
#

# - Missing by size - #

# Missing
missing_by_size <- school_data %>%
  drop_na(school_size) %>%
  group_by(school_size) %>%
  summarise(
    Missing_Count = sum(is.na(share_students_F)),
    Total = n(),
    Share_Missing = round(100 * Missing_Count / Total, 2)
  ) %>%
  arrange(desc(Missing_Count))

missing_by_size %>%
  kbl(format = "latex", booktabs = TRUE,
      caption = "Missing Values in Share of Students with F, by School Size",
      align = "lrrr") %>%
  kable_styling(latex_options = c("hold_position"))

#
#
#
#

# ------ Last Filter ------ #

# Modifying DataFrame
school_data <- school_data %>%
  
  # Filter
  filter(school_size != '1-49')

#
#
#
#

# ------ Checking for Outliers ------ #
ggplot(
  as.data.frame(school_data) %>%
    filter(!is.na(share_students_F)) %>%
    mutate(educational_stage = as.factor(as.character(educational_stage))),
  aes(x = educational_stage, y = share_students_F)) +
  
  my_theme + 
  
  geom_boxplot(
    fill = NA,             # No fill (transparent box)
    color = "black",       # Outline and whisker color
    size = 0.25,            # Thickness of box/whisker lines
    outlier.color = "black",
    outlier.shape = 1,
    outlier.size = 1) + 
  
  labs(
    title = "Boxplot of Share of Students with F",
    x = NULL,  # Optional: removes the axis title (since labels make it clear)
    y = "Share of Students with F") +
  
  scale_x_discrete(
    labels = c(
      "grundskola" = "Lower Secondary School Year 9",
      "gymnasieskola" = "Upper Secondary School Year 10"))

#
#
#
#

# ----- Balancing my Data ----- #
# Determine the number of unique academic years in your dataset
n_periods <- school_data %>%
  distinct(academic_year) %>%
  nrow()

# Filter for schools that appear in all academic years
school_data <- school_data %>%
  group_by(school_ID) %>%
  filter(n_distinct(academic_year) == n_periods) %>%
  ungroup()

#
#
#
#

# ------ Checking for Outliers ------ #
ggplot(
  as.data.frame(school_data) %>%
    filter(!is.na(share_students_F)) %>%
    mutate(educational_stage = as.factor(as.character(educational_stage))),
  aes(x = educational_stage, y = share_students_F)) +
  
  my_theme + 
  
  geom_boxplot(
    fill = NA,             # No fill (transparent box)
    color = "black",       # Outline and whisker color
    size = 0.25,            # Thickness of box/whisker lines
    outlier.color = "black",
    outlier.shape = 1,
    outlier.size = 1) + 
  
  labs(
    title = "Boxplot of Share of Students with F",
    x = NULL,  # Optional: removes the axis title (since labels make it clear)
    y = "Share of Students with F") +
  
  scale_x_discrete(
    labels = c(
      "grundskola" = "Lower Secondary School Year 9",
      "gymnasieskola" = "Upper Secondary School Year 10"))

#
#
#
#

# ------ Converting to Panel Data Structure ------ #

# Panel Data Conversion
school_data <- pdata.frame(school_data, index = c("school_ID", "academic_year"))
is.pbalanced(school_data)

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
  summarise(average_share_students_F = mean(share_students_F))

# Plot the Time Trend for each educational stage
parallel_trend_plot <- ggplot(time_trend, 
                              mapping=aes(x=academic_year_spring, y=average_share_students_F, 
                                          group=educational_stage, color=educational_stage, shape=educational_stage)) + 
  
  # Vertical reference line
  geom_vline(xintercept = 19.5, linetype = "solid", color = "brown", size = 0.25) +
  
  # Data lines and points
  geom_line(size = 0.25) +
  geom_point(size = 1) +
  
  # Custom color and shape assignments
  scale_color_manual(values = c("grundskola" = "darkgrey", "gymnasieskola" = "black"),
                     labels = c("grundskola" = "Lower Secondary, Grade 9", 
                                "gymnasieskola" = "Upper Secondary, Grade 10")) + 
  scale_shape_manual(values = c("grundskola" = 21, "gymnasieskola" = 21),
                     labels = c("grundskola" = "Lower Secondary, Grade 9", 
                                "gymnasieskola" = "Upper Secondary, Grade 10")) +
  
  # Custom x-axis with manually specified breaks and labels
  scale_x_continuous(breaks = c(15, 16, 17, 18, 19, 20, 21, 22),
                     labels = c("14/15", "15/16", "16/17", "17/18", "18/19", "19/20", "20/21", "21/22")) +
  
  # Axis labels and y-axis limits
  xlab("Academic Year") +
  ylab("Average percent of students with F") +
  
  # Custom theme
  my_theme

# Print the plot
print(parallel_trend_plot)

# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output, "Average_DiD_plot.png")

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

# - Unconditional Dynamic Difference-in-Difference Model - 
dynamic_DiD_without <- plm(share_students_F ~ academic_year*treatment_group, 
                           model = "pooling",
                           effect = "individual", 
                           data = school_data)

# Clustered SE by school
dynamic_DiD_without_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_without, type = "HC1", cluster = "group")))

# Summary
dynamic_DiD_without_summary <- summary(dynamic_DiD_without)

# - Conditional Dynamic Difference-in-Difference Model - 
dynamic_DiD_with <- plm(share_students_F ~ academic_year*(treatment_group + private_ownership) + 
                          share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
                        model = "within",   
                        effect = "twoways",
                        data = school_data)

# Clustered SE by school
dynamic_DiD_with_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_with, type = "HC1", cluster = "group")))

# Summary
dynamic_DiD_with_summary <- summary(dynamic_DiD_with)

#
#
#
#
#
#

# -- Step 2 Table Output -- #

# List of models
Dynamic_DiD_models <- list(dynamic_DiD_without, dynamic_DiD_with)

# List of clustered standard errors 
Dynamic_DiD_cse <- list(dynamic_DiD_without_clustered_se, dynamic_DiD_with_clustered_se)

# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output, "Dynamic_DiD_output.html")

# Preparation for Stargazer
years <- c("14/15", "15/16", "16/17", "17/18", "18/19", "19/20", "20/21", "21/22") # Define the academic years to keep
keep_vars <- paste0("academic_year", years, ":treatment_group", collapse = "|") # Construct the 'keep' argument dynamically

# Generate the Stargazer table dynamically using the 'keep' argument
stargazer::stargazer(
  
  # List of Models
  Dynamic_DiD_models,
  
  # List of Clustered Standard Errors
  se = Dynamic_DiD_cse,      
  
  # Title
  title = "Dynamic Difference-in-Difference Models",
  
  # Table Formatting 
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "",
  dep.var.labels = "Share of Student with F in English",
  
  # Variables to keep (dynamic)
  keep = keep_vars,
  
  # Renaming Variables (dynamic)
  covariate.labels = c("Year -5", "Year -4"," Year -3", "Year -2", "Year 0", "Year 1", "Year 2"),
  
  # Three Digits
  digits = 3,
  
  # Omitting some statistics
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  
  # Custom Row Lines
  add.lines = list(
    c("School Controls", "No", "Yes"), 
    c("School Fixed Effects", "No", "Yes"),
    c("Time Fixed Effects", "Yes", "Yes")
  ),
  
  # Custom Column Headers
  column.labels = c("Unconditional", "Conditional"),
  column.separate = c(1, 1),   # One model per label
  
  # Changing the notes section
  notes.append = FALSE, # Exclude default significance levels text
  notes.label = "Note:", # Notes label
  notes = "The academic year 2018/19 is the reference year. Errors are clustered at the school level. Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
  
  # Output settings
  type = 'html',
  out = output_file # Save the table as an HTML file
)

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
  standard_errors = c(dynamic_DiD_without_clustered_se[plot_order], 0),
  label = c(-5, -4, -3, -2, 0, 1, 2, -1),
  model = "Without Controls"
)

# Extracting coefficients for the model WITH controls
dynamic_DiD_with_result <- tibble(
  estimates = c(dynamic_DiD_with_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_with_clustered_se[plot_order], 0),
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
output_file <- file.path(path_output, "Dynamic_DiD_plot.png")

# Save the plot
ggsave(output_file, dynamic_did_plot, bg = "transparent", width=4, height=3, dpi = 1800)

#
#
#
#
#
#

# ---- Static Difference-in-Difference ---- #

# -- Step 1 Modelling -- # 

# - Unconditional Static Difference-in-Difference Model -
model_1 <- plm(share_students_F ~ treatment_year + treatment_group + DiD, 
                        model = "pooling",   
                        effect = "individual",
                        data = school_data)
cse_1 <- sqrt(diag(vcovHC(model_1, type = "HC1", cluster = "group"))) # Clustered Standard Errors

# - Conditional Static Difference-in-Difference Model - 
model_6 <- plm(share_students_F ~ treatment_year + treatment_group + DiD + 
                 academic_year*private_ownership + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
               model = "within",   
               effect = "twoways",
               data = school_data)
cse_6 <- sqrt(diag(vcovHC(model_6, type = "HC1", cluster = "group"))) # Clustered Standard Errors

summary(model_6)
#
#
#
#
#
#

# -- Step 2 Creating a Table -- # 

# List of models
DiD_models <- list(model_1, model_6)

# List of robust standard errors 
DiD_rse <- list(cse_1, cse_6)

# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output, "Static_DiD_output.html")

# Static Difference-in-Differences Table Output
stargazer::stargazer(
  
  # List of Models
  DiD_models,       
  
  # Clustered Standard Errors
  se = DiD_rse,     
  
  # Title
  title = "Static Difference-in-Difference Models",
  
  # Table Formatting 
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "",
  dep.var.labels = "Share of Students with F in English",
  
  # Variables to keep 
  keep = "DiD",
  
  # Variable Label
  covariate.labels = c("Upper Secondary x Post"),
  
  # Three Digits
  digits = 3,
  
  # Omitting Statistics
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  
  # Custom Row Lines
  add.lines = list(
    c("School Controls", "No", "Yes"), 
    c("School Fixed Effects", "No", "Yes"),
    c("Time Fixed Effects", "No", "Yes")
  ),
  
  # Custom Column Headers
  column.labels = c("Unconditional", "Conditional"),
  column.separate = c(1, 1),   # One model per label
  
  # Notes
  notes.append = FALSE,
  notes.label = "Note:",
  notes = "Errors are clustered at the school level. Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
  
  # Output
  type = 'html',
  out = output_file
)


