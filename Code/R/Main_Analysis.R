# ------ 5350 Thesis in Economics ------ #             
# ---- Andres Cruz (25199) and Klara Holmer (25037) ---- #

# Post Propensity Score Matching

#
#
#
#
#
#
#
#

# -------- SET THE LANGUAGE  -------- #

# Options: "English", "Swedish", or "Mathematics"
language <- "English"

# Map the chosen language to its corresponding suffix.
lang_suffix <- switch(language,
                      "Swedish" = "sv",
                      "English" = "eng",
                      "Mathematics" = "ma",
                      stop("Unsupported language"))

#
#
#
#
#
#
#
#

# -------- Initializing -------- #

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
library(MatchIt)
library(cobalt)
library(fixest)
library(here)
library(did)
library(remotes)
library(HonestDiD)

# Special package for sensitivity analysis
# Turn off warning-error-conversion, because the tiniest warning stops installation
# Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
# Install HonestDiD from github
#remotes::install_github("asheshrambachan/HonestDiD")

# Set working directory (adjust the path as needed)
setwd("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Processed Data")

# Reading school data excel file 
school_data <- read_excel("School_Data.xlsx", sheet = "All")

#
#
#
#
#
#
#

# -------- Data Preparation  -------- #

# Modifying DataFrame
school_data <- school_data %>%
  
  # Variable Modification
  mutate(
    
    # Difference in Difference Variables
    treatment_year = ifelse(academic_year %in% c("2019/20", "2020/21"), 1, 0),
    treatment_group = ifelse(educational_stage == "gymnasieskola", 1, 0),
    DiD = treatment_year * treatment_group,
    
    # Dummies
    private_ownership = ifelse(type_of_ownership == "Enskild", 1, 0),
    high_foreign_background = ifelse(share_foreign_background > 50, 1, 0),
    high_postsecondary_parents = ifelse(share_postsecondary_parents > 50, 1, 0),
    high_active_certified_teachers = ifelse(share_active_certified_teachers > 50, 1, 0),
    high_female_students = ifelse(share_female_students > 50, 1, 0),
    high_share_active_certified_teachers = ifelse(share_active_certified_teachers > 50, 1, 0),
    
    # Reformating academic_year
    academic_year = paste0(substring(academic_year, 3, 4), substring(academic_year, 5, 7)),
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
#
#

# -------- Sourcing  -------- #

# 1 Run Themes Code  
source("/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Code/R/Themes.R")

# 2 Run Analysis Pre Matching Code 
source("/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Code/R/Analysis_Pre_Matching.R")

# 3 Run Propensity Score Matching Code
source("/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Code/R/Propensity_Score_Matching.R")

#
#
#
#
#
#
#
#

# -------- Post Summary Statistics -------- #

# Summary stats by treatment group and period
period_stats_full <- school_data %>%
  filter(!is.na(period)) %>%
  group_by(treatment_group, period) %>%
  summarise(
    Fsv_mean      = mean(share_students_F_sv, na.rm = TRUE),
    Fsv_sd        = sd(  share_students_F_sv, na.rm = TRUE),
    Feng_mean     = mean(share_students_F_eng, na.rm = TRUE),
    Feng_sd       = sd(  share_students_F_eng, na.rm = TRUE),
    Fma_mean      = mean(share_students_F_ma, na.rm = TRUE),
    Fma_sd        = sd(  share_students_F_ma, na.rm = TRUE),
    foreign_mean  = mean(share_foreign_background, na.rm = TRUE),
    foreign_sd    = sd(  share_foreign_background, na.rm = TRUE),
    teachers_mean = mean(share_active_certified_teachers, na.rm = TRUE),
    teachers_sd   = sd(  share_active_certified_teachers, na.rm = TRUE),
    parents_mean  = mean(share_postsecondary_parents, na.rm = TRUE),
    parents_sd    = sd(  share_postsecondary_parents, na.rm = TRUE),
    females_mean  = mean(share_female_students, na.rm = TRUE),
    females_sd    = sd(  share_female_students, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(treatment_group, period),
    names_to = c("variable", "stat"),
    names_sep = "_"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(mean_sd = sprintf("%.2f (%.2f)", mean, sd)) %>%
  select(treatment_group, period, variable, mean_sd) %>%
  pivot_wider(
    names_from = c(treatment_group, period),
    values_from = mean_sd,
    names_sep = "_"
  ) %>%
  mutate(variable = recode(variable,
                           Fsv      = "Failure Rates in Subject Swedish",
                           Feng     = "Failure Rates in Subject English",
                           Fma      = "Failure Rates in Subject Mathematics",
                           foreign  = "Foreign Background",
                           teachers = "Active Certified Teachers",
                           parents  = "Postsecondary Parents",
                           females  = "Female Students"
  )) %>%
  select(variable, `0_Pre`, `0_Post`, `1_Pre`, `1_Post`)

# Ownership counts
ownership_counts_chr <- school_data %>%
  filter(!is.na(period)) %>%
  count(treatment_group, period, private_ownership) %>%
  pivot_wider(
    names_from = c(treatment_group, period),
    values_from = n,
    names_sep = "_"
  ) %>%
  mutate(variable = if_else(private_ownership == 1, 
                            "Private Schools", "Public Schools")) %>%
  select(variable, `0_Pre`, `0_Post`, `1_Pre`, `1_Post`) %>%
  mutate(across(-variable, as.character))

# Unique school counts
unique_schools_chr <- school_data %>%
  filter(!is.na(period)) %>%
  distinct(school_ID, treatment_group, period, private_ownership) %>%
  count(treatment_group, period, private_ownership) %>%
  pivot_wider(
    names_from = c(treatment_group, period),
    values_from = n,
    names_sep = "_"
  ) %>%
  mutate(variable = if_else(private_ownership == 1, 
                            "Private Schools", "Public Schools")) %>%
  select(variable, `0_Pre`, `0_Post`, `1_Pre`, `1_Post`) %>%
  mutate(across(-variable, as.character))

# Observation counts
counts_total_chr <- school_data %>%
  filter(!is.na(period)) %>%
  count(treatment_group, period) %>%
  pivot_wider(
    names_from = c(treatment_group, period),
    values_from = n,
    names_sep = "_"
  ) %>%
  mutate(variable = "Observations") %>%
  select(variable, `0_Pre`, `0_Post`, `1_Pre`, `1_Post`) %>%
  mutate(across(-variable, as.character))

# Combine all rows
final_table <- bind_rows(
  period_stats_full,
  ownership_counts_chr,
  unique_schools_chr,
  counts_total_chr
)

# Render as LaTeX table
latex_code <- kable(
  final_table,
  format    = "latex",
  booktabs  = TRUE,
  caption   = "Summary Statistics by Educational Level: Pre and Post Remote Instruction Periods",
  label     = "tab:mamatched_summary",
  col.names = c(
    "Outcome / Count",
    "Ctrl Pre", "Ctrl Post",
    "Trt Pre",  "Trt Post"
  ),
  align  = c("l","r","r","r","r"),
  escape = FALSE
)

# Define the output file path
out_file <- file.path(path_output, "summary_statistics_matched.text")

# Write the LaTeX table to that file
writeLines(latex_code, con = out_file)

# -------- Unique School Municipality Count -------- #

# Extract and sort unique municipality names
municipality_names <- school_data %>%
  distinct(school_municipality) %>%
  arrange(school_municipality) %>%
  pull(school_municipality)

# Format as LaTeX itemized list
latex_list <- paste0("\\item ", municipality_names)
# Wrap with LaTeX environment
latex_list_full <- c("\\begin{itemize}", latex_list, "\\end{itemize}")

# Define output file
out_file <- file.path(path_output, "municipality_list.text")

# Write the LaTeX code to file
writeLines(latex_list_full, con = out_file)

#
#
#
#
#
#
#
#

# ---- Estimating the Effect of the Pandemic for Grade 10 Students ---- #

# Panel Data Conversion
school_data <- pdata.frame(school_data, index = c("school_ID", "academic_year"))

# -- Swedish -- # 
# Unconditional Model
model_1_sv <- plm(
  share_students_F_sv ~ DiD, 
  model = "pooling",  data = school_data)
cse_1_sv <- sqrt(diag(vcovHC(model_1_sv, type = "HC1", cluster = "group"))) # Clustered Standard Errors
# Conditional Model 
model_6_sv <- plm(
  share_students_F_sv ~ DiD + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "pooling", data = school_data)
cse_6_sv <- sqrt(diag(vcovHC(model_6_sv, type = "HC1", cluster = "group"))) # Clustered Standard Errors

# -- English -- # 
# Unconditional Model
model_1_eng <- plm(
  share_students_F_eng ~ DiD, 
  model = "pooling",  data = school_data)
cse_1_eng <- sqrt(diag(vcovHC(model_1_eng, type = "HC1", cluster = "group"))) # Clustered Standard Errors
# Conditional Model 
model_6_eng <- plm(
  share_students_F_eng ~ DiD + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "pooling", data = school_data)
cse_6_eng <- sqrt(diag(vcovHC(model_6_eng, type = "HC1", cluster = "group"))) # Clustered Standard Errors

# -- Mathematics -- # 
# Unconditional Model
model_1_ma <- plm(
  share_students_F_ma ~ DiD, 
  model = "pooling",  data = school_data)
cse_1_ma <- sqrt(diag(vcovHC(model_1_ma, type = "HC1", cluster = "group"))) # Clustered Standard Errors
# Conditional Model 
model_6_ma <- plm(
  share_students_F_ma ~ DiD + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "pooling", data = school_data)
cse_6_ma <- sqrt(diag(vcovHC(model_6_ma, type = "HC1", cluster = "group"))) # Clustered Standard Errors


# -- Step 2 Creating a Table -- # 

# List of models
DiD_models <- list(model_1_sv, model_6_sv, model_1_eng, model_6_eng, model_1_ma, model_6_ma)

# List of robust standard errors 
DiD_rse <- list(cse_1_sv, cse_6_sv, cse_1_eng, cse_6_eng, cse_1_ma, cse_6_ma)

# Add the other variables explicitly
keep_vars <- c("DiD",
               "share_foreign_background",
               "share_active_certified_teachers",
               "share_postsecondary_parents",
               "share_female_students")

# Covariate Labels
covariate_labels <- c("Grade 10 x Post", 
                      "Foreign Background", 
                      "Active Certified Teachers", 
                      "Postsecondary Parents", 
                      "Female Students")


# Save the table 
path_output <- file.path("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/Propensity Score Matching")
output_file <- file.path(path_output, paste0("static_did_output_all.text"))

# Stargazer
stargazer::stargazer(
  
  # List of Models
  DiD_models,
  
  # List of Clustered Standard Errors
  se = DiD_rse,      
  
  # Title
  title = "Difference-in-Difference Models",
  
  # Table Formatting 
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "Failure Rates",
  
  # Keep only rows that match either the interaction (contains a colon)
  # or the share_ variables
  keep = keep_vars,
  
  # Covariate Labels
  covariate.labels = covariate_labels,
  
  # Three Digits
  digits = 3,
  
  # Omitting some statistics
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  
  # Custom Row Lines
  add.lines = list(
    c("School Fixed Effects", "No", "No", "No", "No", "No", "No"),
    c("Time Fixed Effects", "No", "No", "No", "No", "No", "No")),
  
  # Removing the notes section
  omit.table.layout = "n",
  
  # Output settings
  type = 'latex',
  out = output_file
)

#
#
#
#
#
#
#
#


# -------- Initializing Dynamic Code-------- #

# Drop the pdata.frame class
class(school_data) <- "data.frame"

# Define the output path dynamically based on the language variable
path_output <- file.path("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output", language)

# Construct the old column name dynamically using the suffix
share_student_F_lang_suffix <- paste0("share_students_F_", lang_suffix)

# RENAMING
school_data <- school_data %>%
  rename(share_students_F = all_of(share_student_F_lang_suffix))

#
#
#
#
#
#
#
#


# -------- Parallel Trend Assumption -------- #

# ---- Time Trends ---- #

# Time Trend
time_trend <- school_data %>% 
  group_by(academic_year_spring, educational_stage) %>% 
  summarise(average_share_students_F = mean(share_students_F))

# Plot the Time Trend for each educational stage
time_trend_plot <- ggplot(time_trend, mapping = aes(x = academic_year_spring, y = average_share_students_F, 
                                                    group = educational_stage, color = educational_stage, shape = educational_stage)) + 
  # Time Trend Theme
  time_trend_theme + 
  # My_theme
  my_theme

# Print the plot
print(time_trend_plot)

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("time_trend_plot_", language, ".png"))
ggsave(output_file, time_trend_plot, width = 5, height = 4)

#
#
#
#

# ---- Dynamic Difference-in-Difference ---- #

# Panel Data Conversion
school_data <- pdata.frame(school_data, index = c("school_ID", "academic_year"))

# Setting the reference year
school_data$academic_year <- relevel(school_data$academic_year, ref = "18/19")

# -- Step 1 Modelling -- #

# - Unconditional Dynamic Difference-in-Difference Model or Model 0 - #
dynamic_DiD_without <- plm(
  share_students_F ~ academic_year*treatment_group, 
  model = "within", effect = "twoways", data = school_data)
# Clustered SE by school
dynamic_DiD_without_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_without, type = "HC1", cluster = "group"))) 
# Summary
dynamic_DiD_without_summary <- summary(dynamic_DiD_without) 

# Model 1 - #
dynamic_model_1 <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background, 
  model = "within", effect = "twoways", data = school_data)
dynamic_model_1_cse <- sqrt(diag(vcovHC(dynamic_model_1, type = "HC1", cluster = "group"))) # Clustered SE by school
dynamic_model_1_summary <- summary(dynamic_model_1) # Summary

# Model 2 - #
dynamic_model_2 <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers, 
  model = "within", effect = "twoways", data = school_data)
dynamic_model_2_cse <- sqrt(diag(vcovHC(dynamic_model_2, type = "HC1", cluster = "group"))) # Clustered SE by school
dynamic_model_2_summary <- summary(dynamic_model_2) # Summary

# Model 3 - #
dynamic_model_3 <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents, 
  model = "within", effect = "twoways", data = school_data)
dynamic_model_3_cse <- sqrt(diag(vcovHC(dynamic_model_3, type = "HC1", cluster = "group"))) # Clustered SE by school
dynamic_model_3_summary <- summary(dynamic_model_3) # Summary

# - Conditional Dynamic Difference-in-Difference Model or Model 4 - #
dynamic_DiD_with <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "within", effect = "twoways", data = school_data)
dynamic_DiD_with_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_with, type = "HC1", cluster = "group"))) # Clustered SE by school
dynamic_DiD_with_summary <- summary(dynamic_DiD_with) # Summary

#
#
#
#

# -- Step 2 Table Output -- #

# List of models
Dynamic_DiD_models <- list(dynamic_DiD_without, 
                           dynamic_model_1, 
                           dynamic_model_2, 
                           dynamic_model_3, 
                           dynamic_DiD_with)

# List of clustered standard errors 
Dynamic_DiD_cse <- list(dynamic_DiD_without_clustered_se, 
                        dynamic_model_1_cse, 
                        dynamic_model_2_cse, 
                        dynamic_model_3_cse, 
                        dynamic_DiD_with_clustered_se)

# Generate the Stargazer table dynamically using the 'keep' argument
years <- c("14/15", "15/16", "16/17", "17/18", "19/20", "20/21") # Define the academic years to keep

# Construct the dynamic interaction terms (already correctly formed)
keep_interactions <- paste0("academic_year", years, ":treatment_group", collapse = "|")

# Add the other variables explicitly
additional_vars <- c("share_foreign_background",
                     "share_active_certified_teachers",
                     "share_postsecondary_parents",
                     "share_female_students")

# Combine interaction terms with additional variables
keep_vars <- paste(c(keep_interactions, additional_vars), collapse = "|")

#  
covariate_labels <- c("Foreign Background", 
                      "Active Certified Teachers", 
                      "Postsecondary Parents", 
                      "Female Students",
                      "2014/15 x Grade 10", 
                      "2015/16 x Grade 10", 
                      "2016/17 x Grade 10", 
                      "2017/18 x Grade 10", 
                      "2019/20 x Grade 10",
                      "2020/21 x Grade 10")

# Save the table to a dynamic file name
output_file <- file.path(path_output, paste0("dynamic_did_output_", language, ".text"))

# Stargazer
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
  dep.var.caption = paste("Failure Rates in Subject", language),  
  dep.var.labels = "",
  
  
  # Keep only rows that match either the interaction (contains a colon)
  # or the share_ variables
  keep = keep_vars,
  
  # Covariate Labels
  covariate.labels = covariate_labels,
  
  # Three Digits
  digits = 3,
  
  # Omitting some statistics
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  
  # Custom Row Lines
  add.lines = list(
    c("School Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes"),
    c("Time Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes")
  ),
  
  # Removing the notes section
  omit.table.layout = "n",
  
  # Output settings
  type = 'latex',
  out = output_file
)

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
                'academic_year20/21:treatment_group') 

# Extracting coefficients for the model WITHOUT controls
dynamic_DiD_without_result <- tibble(
  estimates = c(dynamic_DiD_without_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_without_clustered_se[plot_order], 0),
  label = c(-5, -4, -3, -2, 0, 1, -1),
  model = "Without Controls"
)

# Extracting coefficients for the model WITH controls
dynamic_DiD_with_result <- tibble(
  estimates = c(dynamic_DiD_with_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_with_clustered_se[plot_order], 0),
  label = c(-5, -4, -3, -2, 0, 1, -1),
  model = "With Controls"
)

# Combine the results into one data frame
combined_results <- bind_rows(dynamic_DiD_without_result, dynamic_DiD_with_result)

# Reorder the factor levels so "Without Controls" comes first
combined_results <- combined_results %>%
  mutate(model = factor(model, levels = c("Without Controls", "With Controls")))

# Create the event study plot with overlaid results
dynamic_did_plot <- ggplot(data = combined_results, aes(x = label, y = estimates, color = model, shape = model, group = model)) +
  # Dynamic Did Theme
  dynamic_did_theme + 
  # My Theme
  my_theme

# Print the combined plot
print(dynamic_did_plot)

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("dynamic_did_plot_", language, ".png"))
ggsave(output_file, dynamic_did_plot, width = 5, height = 4)

#
#
#
#
#
#
#
#

# -------- Heterogeneity Analysis  -------- #

# -- Private Ownership -- #
# Private schools (private_ownership == 1)
school_data_private <- subset(school_data, private_ownership == 1)
# Public schools (private_ownership == 0)
school_data_public <- subset(school_data, private_ownership == 0)

# Private  
# Conditional Model on Private Sample
model_private <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "within", effect = "twoways", data = school_data_private)
model_private_cse <- sqrt(diag(vcovHC(model_private, type = "HC1", cluster = "group"))) # Clustered SE by school

# Public  
# Conditional Model on Private Sample
model_public <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "within", effect = "twoways", data = school_data_public)
model_public_cse <- sqrt(diag(vcovHC(model_public, type = "HC1", cluster = "group"))) # Clustered SE by school

#
#
#
#

# -- Foreign Background -- #
# High Foreign Background Schools
school_data_high_foreign <- subset(school_data, high_foreign_background == 1)
# Low Foreign Background Schools
school_data_low_foreign <- subset(school_data, high_foreign_background == 0)

# High  
# Conditional Model on Foreign Sample
model_high_foreign <- plm(
  share_students_F ~ academic_year*treatment_group + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "within", effect = "twoways", data = school_data_high_foreign)
model_high_foreign_cse <- sqrt(diag(vcovHC(model_high_foreign, type = "HC1", cluster = "group"))) # Clustered SE by school
model_high_foreign_summary <- summary(model_high_foreign) # Summary

# Low  
# Conditional Model on Foreign Sample
model_low_foreign <- plm(
  share_students_F ~ academic_year*treatment_group + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "within", effect = "twoways", data = school_data_low_foreign)
model_low_foreign_cse <- sqrt(diag(vcovHC(model_low_foreign, type = "HC1", cluster = "group"))) # Clustered SE by school
model_low_foreign_summary <- summary(model_low_foreign) # Summary

#
#
#
#

# -- Postsecondary Parents -- #
# High Postsecondary Parents Schools
school_data_high_postsecondary <- subset(school_data, high_postsecondary_parents == 1)
# Low Postsecondary Parents Schools
school_data_low_postsecondary <- subset(school_data, high_postsecondary_parents == 0)

# High  
# Conditional Model on Postsecondary Sample
model_high_postsecondary <- plm(
  share_students_F ~ academic_year*treatment_group, 
  model = "within", effect = "twoways", data = school_data_high_postsecondary)
model_high_postsecondary_cse <- sqrt(diag(vcovHC(model_high_postsecondary, type = "HC1", cluster = "group"))) # Clustered SE by school

# Low  
# Conditional Model on Postsecondary Sample
model_low_postsecondary <- plm(
  share_students_F ~ academic_year*treatment_group, 
  model = "within", effect = "twoways", data = school_data_low_postsecondary)
model_low_postsecondary_cse <- sqrt(diag(vcovHC(model_low_postsecondary, type = "HC1", cluster = "group"))) # Clustered SE by school

#
#
#
#

# -- Female Students -- #
# High Postsecondary Parents Schools
school_data_high_female <- subset(school_data, high_female_students == 1)
# Low Postsecondary Parents Schools
school_data_low_female <- subset(school_data, high_female_students == 0)

# High  
# Conditional Model on Female Sample
model_high_female <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents, 
  model = "within", effect = "twoways", data = school_data_high_female)
model_high_female_cse <- sqrt(diag(vcovHC(model_high_female, type = "HC1", cluster = "group"))) # Clustered SE by school

# Low  
# Conditional Model on Female Sample
model_low_female <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents, 
  model = "within", effect = "twoways", data = school_data_low_female)
model_low_female_cse <- sqrt(diag(vcovHC(model_low_female, type = "HC1", cluster = "group"))) # Clustered SE by school

#
#
#
#

# -- Teacher  -- #
# High Teacher Schools
school_data_high_teacher <- subset(school_data, high_share_active_certified_teachers == 1)
# Low Teacher Parents Schools
school_data_low_teacher <- subset(school_data, high_share_active_certified_teachers == 0)

# High  
# Conditional Model on Teacher Sample
model_high_teacher <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background + share_postsecondary_parents + share_female_students, 
  model = "within", effect = "twoways", data = school_data_high_teacher)
model_high_teacher_cse <- sqrt(diag(vcovHC(model_high_teacher, type = "HC1", cluster = "group"))) # Clustered SE by school

# Low  
# Conditional Model on Teacher Sample
model_low_teacher <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background + share_postsecondary_parents + share_female_students, 
  model = "within", effect = "twoways", data = school_data_low_teacher)
model_low_teacher_cse <- sqrt(diag(vcovHC(model_low_teacher, type = "HC1", cluster = "group"))) # Clustered SE by school

#
#
#
#

# -- Creating a Table -- # 

# List of models
models <- list(model_private, model_public, 
               model_high_foreign, model_low_foreign, 
               model_high_postsecondary, model_low_postsecondary,
               model_high_female, model_low_female,
               model_high_teacher, model_low_teacher)

# List of robust standard errors 
cse <- list(model_private_cse, model_public_cse,
            model_high_foreign_cse, model_low_foreign_cse,
            model_high_postsecondary_cse, model_low_postsecondary_cse,
            model_high_female_cse, model_low_female_cse,
            model_high_teacher_cse, model_low_teacher_cse)

# Keep Interaction
years <- c("14/15","15/16","16/17","17/18","19/20","20/21")
interactions <- paste0("academic_year", years, ":treatment_group")

# Covariate Labels
covariate_labels <- c("2014/15 x Grade 10", 
                      "2015/16 x Grade 10", 
                      "2016/17 x Grade 10", 
                      "2017/18 x Grade 10", 
                      "2019/20 x Grade 10",
                      "2020/21 x Grade 10")


# Save the table to a dynamic file name
output_file <- file.path(path_output, paste0("heterogeneity_did_output_", language, ".text"))

# Stargazer
stargazer::stargazer(
  
  # List of Models
  models,
  
  # List of Clustered Standard Errors
  se = cse,      
  
  # Title
  title = "Heterogeneous Effects",
  
  # Group Headers Above Model Numbers
  column.labels   = c("Private", "Public", 
                      "High Foreign Share", "Low Foreign Share", 
                      "High Postsecondary Share", "Low Postsecondary Share",
                      "High Female Share", "Low Female Share",
                      "High Teacher, Low Teacher Share"),
  column.separate = rep(1, 10),
  
  # Table Formatting 
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = paste("Failure Rates in Subject", language),  
  dep.var.labels = "",
  
  # Keep only rows that match either the interaction (contains a colon)
  # or the share_ variables
  keep = keep_interactions,
  
  # Covariate Labels
  covariate.labels = covariate_labels,
  
  # Three Digits
  digits = 3,
  
  # Omitting some statistics
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  
  add.lines = list(
    c("School Fixed Effects", rep("Yes", 10)),
    c("Time Fixed Effects",   rep("Yes", 10)),
    c("Controls",             rep("Yes", 10))),
  
  # Removing the notes section
  omit.table.layout = "n",
  
  # Output settings
  type = 'latex',
  out = output_file
)

#
#
#
#
#
#
#
#

# -------- Sensitivity Analysis following Rambachan and Roth (2022)  -------- #

# Method: https://www.jonathandroth.com/assets/files/DiD_Review_Paper.pdf
# Paper: https://www.jonathandroth.com/assets/files/HonestParallelTrends_Main.pdf
# Code: https://raw.githack.com/Mixtape-Sessions/Advanced-DID/main/Exercises/Exercise-2/Solutions/medicaid-analysis-pt-violations-solutions-R.html
# Code: https://github.com/asheshrambachan/HonestDiD



# ---- Step 0 Modifying data ---- #

# Drop the pdata.frame class
class(school_data) <- "data.frame"

# Rebuild as a proper pdata.frame
school_data <- pdata.frame(school_data, index = c("school_ID", "academic_year"))

# Filtering academic years and Resetting reference year to the baseline
school_data$academic_year <- factor(school_data$academic_year, levels = c("18/19", "14/15", "15/16", "16/17", "17/18", "19/20", "20/21")) 
levels(school_data$academic_year) # Check if it worked


# -- Step 1 Modelling -- #

# Coefficients and Standard Errors names
plot_order <- c(
  "academic_year14/15:treatment_group",
  "academic_year15/16:treatment_group",
  "academic_year16/17:treatment_group",
  "academic_year17/18:treatment_group",
  "academic_year19/20:treatment_group", 
  "academic_year20/21:treatment_group")

# Extract the coefficients from the Conditional Dynamic DiD Model
dynamic_DiD_with <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "within", 
  effect = "twoways",
  data = school_data)
dynamic_DiD_with_summary <- summary(dynamic_DiD_with) # Summary
betahat <- dynamic_DiD_with_summary$coefficients[plot_order, "Estimate"] # BetaHat

# Constructing and extract the standard errors for the Conditional Dynamic DiD Model
vcov_full <- vcovHC(dynamic_DiD_with, type = "HC1", cluster = "group") # Clustering Standard Errors
sigma <- vcov_full[plot_order, plot_order] # Sigma

# - Relative Magnitudes Restriction in the first year of treatment - #

# Original
originalResults <- HonestDiD::constructOriginalCS(
  betahat = betahat,  #coefficients
  sigma = sigma, #covariance matrix
  numPrePeriods = 4, #num. of pre-treatment coefs
  numPostPeriods = 2  #num. of post-treatment coefs 
  )

# Delta RM 
delta_rm_results <-
  HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = betahat, #coefficients
    sigma = sigma, #covariance matrix
    numPrePeriods = 4, #num. of pre-treatment coefs
    numPostPeriods = 2, #num. of post-treatment coefs 
    Mbarvec = seq(0.5, 2, by = 0.5), #values of Mbar
    l_vec = basisVector(index = 1, size = 2)
    )

# Sensitivity Plot
sensitivity_plot_rm <- HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults) + 
  # Changing colors for each group
  scale_color_manual(
    values = c("Original" = "darkgrey", "C-LF" = "black"), 
    breaks = c("Original", "C-LF") # this sets the legend order explicitly
  ) + 

  # Add a geom_ribbon layer for shading the error intervals
  geom_ribbon(aes(x = Mbar, ymin = lb[,1], ymax = ub[,1]),
              fill = "lightgrey", alpha = 0.25, inherit.aes = TRUE) + 
  # Adding my theme
  my_theme

# More customization 
# Vertical M Lines
sensitivity_plot_rm$layers[[1]]$geom_params$width <- 0.05
sensitivity_plot_rm$layers[[1]]$geom_params$size <- 0.25
# Horizontal Zero Line
sensitivity_plot_rm$layers[[2]]$aes_params$linetype <- "solid"
sensitivity_plot_rm$layers[[2]]$geom_params$size <- 0.25
sensitivity_plot_rm$layers[[2]]$aes_params$colour <- "brown1"
# Print Results
sensitivity_plot_rm

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("rm_plot_first_", language, ".png"))
ggsave(output_file, sensitivity_plot_rm, width = 4, height = 3)

#
#
#
#
#
#

# - Relative Magnitudes Restriction for Average Effects - #
# Original
originalResults_avg <- HonestDiD::constructOriginalCS(betahat = betahat,
                                                      sigma = sigma,
                                                      numPrePeriods = 4,
                                                      numPostPeriods = 2,
                                                      l_vec = c(0.5,0.5))
# Delta RM 
delta_rm_results_avg <- HonestDiD::createSensitivityResults_relativeMagnitudes(betahat = betahat,
                                                                               sigma = sigma,
                                                                               numPrePeriods = 4,
                                                                               numPostPeriods = 2, Mbarvec = seq(0.5, 2, by=0.5),
                                                                               l_vec = c(0.5,0.5))


# Sensitivity Plot
sensitivity_plot_rm <- HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_results_avg, originalResults_avg) + 
  # Changing colors for each group
  scale_color_manual(
    values = c("Original" = "darkgrey", "C-LF" = "black"), 
    breaks = c("Original", "C-LF") # this sets the legend order explicitly
  ) + 
  
  # Add a geom_ribbon layer for shading the error intervals
  geom_ribbon(aes(x = Mbar, ymin = lb[,1], ymax = ub[,1]),
              fill = "lightgrey", alpha = 0.25, inherit.aes = TRUE) + 
  # Adding my theme
  my_theme

# More customization 
# Vertical M Lines
sensitivity_plot_rm$layers[[1]]$geom_params$width <- 0.05
sensitivity_plot_rm$layers[[1]]$geom_params$size <- 0.25
# Horizontal Zero Line
sensitivity_plot_rm$layers[[2]]$aes_params$linetype <- "solid"
sensitivity_plot_rm$layers[[2]]$geom_params$size <- 0.25
sensitivity_plot_rm$layers[[2]]$aes_params$colour <- "brown1"
# Print Results
sensitivity_plot_rm

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("rm_plot_avg_", language, ".png"))
ggsave(output_file, sensitivity_plot_rm, width = 4, height = 3)