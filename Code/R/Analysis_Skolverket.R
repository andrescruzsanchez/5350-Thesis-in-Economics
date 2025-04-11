# ------ 5350 Thesis in Economics ------ #             
# ---- Andres Cruz (25199) and Klara Holmer (25037) ---- #

#
#
#
#
#
#
#
#

# -------- Initializing -------- #

# Clear workspace
rm(list=ls()) 

# Clear console
cat("\014") 

# Set working directory
setwd("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Processed Data")

# Source Customed Themes  
source("/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Code/R/themes.R")

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

#
#
#
#
#
#
#
#

# -------- Data Preparation -------- #

# ---- Variables Modification ---- #

# CHANGE LANGUAGE HERE
# "English" (eng), "Swedish" (sv), or "Mathematics" (ma)
language <- "Swedish"

# Reading school data excel file and importing sheet based on language variable
school_data <- read_excel("school_data_ämnen.xlsx", sheet = language)

# Define the path to the output folder dynamically based on the language variable
path_output <- file.path("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output", language)

# Modifying DataFrame
school_data <- school_data %>%
  
  # CHANGE LANGUAGE HERE
  # "English" (eng), "Swedish" (sv), or "Mathematics" (ma)
  rename(share_students_F = share_students_F_sv,
         share_students_A_E = share_students_A_E_sv) %>%
  
  # Drop unused variable
  select(-share_students_A_E) %>%
  
  # Variable Modification
  mutate(
    
    # Difference in Difference Variables
    treatment_year = ifelse(academic_year == "2019/20", 1, 0), 
    treatment_group = ifelse(educational_stage == "gymnasieskola", 1, 0),
    DiD = treatment_year * treatment_group,
    
    # Control Variables
    private_ownership = ifelse(type_of_ownership == "Enskild", 1, 0),
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

# ---- Filter ---- #

# Missing by share_students_F and school_size 
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

# Filtering out smallest schools
school_data <- school_data %>% 
  filter(school_size != '1-49')

#
#
#
#

# ---- Balancing ---- #
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
#
#
#
#

# -------- Propensity Score Matching -------- #

# Reference I: https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html#ref-vanderweele2019
# Reference II: https://simonejdemyr.com/r-tutorials/statistics/tutorial8.html
# Reference III: https://rpubs.com/Paul_Rucki/934261
# Reference IIII: https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html#generalized-full-matching-method-quick
# Reference IIIII: https://dlab.berkeley.edu/news/introduction-propensity-score-matching-matchit

# Filter for pre-treatment period only
school_data_pre <- school_data %>%
  filter(academic_year == '18/19') %>% 
  drop_na(graduates_average_grade_points, share_active_certified_teachers, share_postsecondary_parents, share_foreign_background, share_female_students)

# No Matching
unmatched_object <- matchit(
  treatment_group ~ graduates_average_grade_points + private_ownership + share_active_certified_teachers + share_postsecondary_parents + share_foreign_background + share_female_students, 
  data = school_data_pre,
  method = NULL,
  distance = 'glm',
  link = 'logit')

# Check Covariate Imbalance
summary(unmatched_object) 

# Matching,  constructing a matched object
matched_object <- matchit(
  treatment_group ~ graduates_average_grade_points + private_ownership + share_active_certified_teachers + share_postsecondary_parents + share_foreign_background + share_female_students, 
  data = school_data_pre,
  method = 'nearest', 
  distance = 'glm',
  link = 'logit',            # Estimating propensity scores using logistic regression (default)
  m.order = "largest",       # Start matching with treated units that have the highest propensity scores (not default)
  ratio = 1,                 # Match each treated unit to 2 control (default)
  replace = TRUE,           # Not allowing to reuse of control units (default)
  exact = ~private_ownership + school_municipality # Match only within private/public school and within each municipality
)

# Checking balance after NN matching
# Summary
summary(matched_object, un = FALSE) 
print(matched_object)

# Love Plot
love_plot <- love.plot(matched_object, 
                       var.names = c("distance" = "Distance", 
                                     "graduates_average_grade_points" = "Average Grade Points",
                                     "private_ownership" = "Private Ownership",
                                     "share_active_certified_teachers" = "Proportion of Active Certified Teachers",
                                     "share_postsecondary_parents" = "Proportion of Students with Postsecondary Parents", 
                                     "share_foreign_background" = "Proportion of Students with Foreign Background",
                                     "share_female_students" = "Proportion of Female Students"
                                     )) +
  scale_color_manual(values = c("Unadjusted" = "darkgrey", "Adjusted" = "black")) +
  labs(title = NULL) +
  my_theme
love_plot

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("love_plot_", language, ".png"))
ggsave(output_file, love_plot, bg = 'transparent', width = 5, height = 3)

# Histogram
histogram_plots <- plot(matched_object, type='hist') 
histogram_plots

# Saving Histogram
# Define the dynamic output file name.
output_file <- file.path(path_output, paste0("histogram_plot_", language, ".png"))
# Open a PNG device.
# Open a PNG device with your desired dimensions.
png(filename = output_file)
# Reproduce the histogram plot.
plot(matched_object, type = 'hist')
# Close the device to complete the saving process.
dev.off()

# - Convert matched object to data set - 
school_data_matched <- match.data(matched_object)

# - Filter original panel data to matched schools only - 
school_data <- school_data %>%
  filter(school_ID %in% school_data_matched$school_ID) 

#
#
#
#
#
#
#
#

# -------- Summary Statistics -------- #

# ---- Ownership ---- #

# Prepare the summary table, grouped also by educational_stage
ownership_table <- school_data %>%
  group_by(academic_year, private_ownership, educational_stage) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(
    names_from = private_ownership,
    values_from = Count,
    names_prefix = "Private_ownership_"
  ) %>%
  arrange(educational_stage, academic_year)

# Transpose the table for LaTeX output
ownership_table_t <- ownership_table %>%
  pivot_longer(
    cols = starts_with("Private_ownership_"),
    names_to = "Ownership Type",
    values_to = "Count"
  ) %>%
  pivot_wider(
    names_from = academic_year,
    values_from = Count
  ) %>%
  arrange(educational_stage, `Ownership Type`)

# Create LaTeX table
ownership_table_t %>%
  kbl(format = "latex", booktabs = TRUE,
      caption = "Number of Schools by Educational Stage, Ownership Type, and Academic Year") %>%
  kable_styling(latex_options = c("hold_position"))

#
#
#
#

# ---- Educational Stage ---- #

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

# ---- Balanced boxplot ---- #
box_plot <- ggplot(
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
    title = NULL,
    x = NULL,  # Optional: removes the axis title (since labels make it clear)
    y = "Share of Students with F") +
  
  scale_x_discrete(
    labels = c(
      "grundskola" = "Lower Secondary, Grade 9",
      "gymnasieskola" = "Upper Secondary, Grade 10"))

# Print the plot
box_plot

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("box_plot_", language, ".png"))
ggsave(output_file, box_plot, bg = 'transparent', width = 4, height = 3)

#
#
#
#
#
#
#
#

# -------- Converting to Panel Data Structure -------- #

# Panel Data Conversion
school_data <- pdata.frame(school_data, index = c("school_ID", "academic_year"))
is.pbalanced(school_data)

#
#
#
#
#
#
#
#

# -------- Parallel Trend Assumption -------- #

# ---- Parallel Trend Plot ---- #

# Time Trend
time_trend <- school_data %>% 
  drop_na(share_students_F) %>% 
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
ggsave(output_file, time_trend_plot, width = 4, height = 3)

#
#
#
#

# ---- Dynamic Difference-in-Difference ---- #

# Setting the reference year
school_data$academic_year <- relevel(school_data$academic_year, ref = "18/19")

# -- Step 1 Modelling -- #

# - Unconditional Dynamic Difference-in-Difference Model - #
dynamic_DiD_without <- plm(
  share_students_F ~ academic_year*treatment_group, 
  model = "pooling",
  effect = "individual", 
  data = school_data)
# Clustered SE by school
dynamic_DiD_without_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_without, type = "HC1", cluster = "group"))) 
# Summary
dynamic_DiD_without_summary <- summary(dynamic_DiD_without) 

# - Conditional Dynamic Difference-in-Difference Model - #
dynamic_DiD_with <- plm(
  share_students_F ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "within", 
  effect = "individual",
  data = school_data)
# Clustered SE by school
dynamic_DiD_with_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_with, type = "HC1", cluster = "group"))) 
# Summary
dynamic_DiD_with_summary <- summary(dynamic_DiD_with) 

#
#

# -- Step 2 Table Output -- #

# List of models
Dynamic_DiD_models <- list(dynamic_DiD_without, dynamic_DiD_with)

# List of clustered standard errors 
Dynamic_DiD_cse <- list(dynamic_DiD_without_clustered_se, dynamic_DiD_with_clustered_se)

# Generate the Stargazer table dynamically using the 'keep' argument
years <- c("14/15", "15/16", "16/17", "17/18", "18/19", "19/20", "20/21", "21/22") # Define the academic years to keep

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
covariate_labels <- c("Foreign Background", "Active Certified Teachers", "Postsecondary Parents", "Female Students", "Year -5", "Year -4", "Year -3", "Year -2", "Year 0", "Year 1", "Year 2")

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
  dep.var.caption = "",
  dep.var.labels = paste("Share of Students with F in", language),
  
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
    c("School Fixed Effects", "No", "Yes"),
    c("Time Fixed Effects", "Yes", "Yes")
  ),
  
  # Custom Column Headers
  column.labels = c("Unconditional", "Conditional"),
  column.separate = c(1, 1),
  
  # Changing the notes section
  notes.append = FALSE,
  notes.label = "Note:",
  notes = "The academic year 2018/19 is the reference year. Errors are clustered at the school level. Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
  
  # Output settings
  type = 'latex',
  out = output_file
)

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
  # Dynamic Did Theme
  dynamic_did_theme + 
  # My Theme
  my_theme

# Print the combined plot
print(dynamic_did_plot)

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("dynamic_did_plot_", language, ".png"))
ggsave(output_file, dynamic_did_plot, width = 4, height = 3)

#
#
#
#
#
#
#
#


# ---- Filter ---- #

school_data <- school_data %>% 
  filter(!academic_year %in% c("20/21", "21/22")) # Use actual values present in the data

#
#
#
#
#
#
#
#

# ---- Canonical Difference-in-Difference with Multiple Time Periods ---- #

# -- Step 1 Modelling -- # 

# - Unconditional Difference-in-Difference Model -
model_1 <- plm(
  share_students_F ~ treatment_year + treatment_group + DiD, 
  model = "pooling",   
  effect = "individual",
  data = school_data)
cse_1 <- sqrt(diag(vcovHC(model_1, type = "HC1", cluster = "group"))) # Clustered Standard Errors

# - Conditional Difference-in-Difference Model - TWFE
model_6 <- plm(
  share_students_F ~ DiD + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students + academic_year, 
  model = "within",   
  effect = "individual",
  data = school_data)
cse_6 <- sqrt(diag(vcovHC(model_6, type = "HC1", cluster = "group"))) # Clustered Standard Errors

#
#

# -- Step 2 Creating a Table -- # 

# List of models
DiD_models <- list(model_1, model_6)

# List of robust standard errors 
DiD_rse <- list(cse_1, cse_6)

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


# Save the table to a dynamic file name
output_file <- file.path(path_output, paste0("canonical_did_output_", language, ".tex"))

# Static Difference-in-Differences Table Output
stargazer::stargazer(
  
  # List of Models
  DiD_models,       
  
  # Clustered Standard Errors
  se = DiD_rse,     
  
  # Title
  title = "Canonical Difference-in-Difference Models with Multiple Time Periods",
  
  # Table Formatting 
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "",
  dep.var.labels = paste("Share of Students with F in", language),
  
  # Variables to keep 
  keep = keep_vars,
  
  # Variable Label
  covariate.labels = covariate_labels,
  
  # Three Digits
  digits = 3,
  
  # Omitting Statistics
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  
  # Custom Row Lines
  add.lines = list(
    c("School Fixed Effects", "No", "Yes"),
    c("Time Fixed Effects", "No", "Yes")
  ),
  
  # Custom Column Headers
  column.labels = c("Unconditional", "Conditional"),
  column.separate = c(1, 1),
  
  # Notes
  notes.append = FALSE,
  notes.label = "Note:",
  notes = "Errors are clustered at the school level. Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
  
  # Output
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

# -- Step 1 Modelling -- #

# Coefficients and Standard Errors names
plot_order <- c(
  "academic_year14/15:treatment_group",
  "academic_year15/16:treatment_group",
  "academic_year16/17:treatment_group",
  "academic_year17/18:treatment_group",
  "academic_year19/20:treatment_group")

# Extract the coefficients from the Conditional Dynamic DiD Model
dynamic_DiD_with <- plm(
  share_students_F ~ academic_year * treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "within", 
  effect = "individual",
  data = school_data)
dynamic_DiD_with_summary <- summary(dynamic_DiD_with) # Summary
betahat <- dynamic_DiD_with_summary$coefficients[plot_order, "Estimate"] # BetaHat

# Constructing and extract the standard errors for the Conditional Dynamic DiD Model
vcov_full <- vcovHC(dynamic_DiD_with, type = "HC1", cluster = "group") # Clustering Standard Errors
sigma <- vcov_full[plot_order, plot_order] # Sigma

#
#

# - Relative Magnitudes Restriction - #

# Delta RM 
delta_rm_results <-
  HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = betahat,
    sigma = sigma,
    numPrePeriods = 4,
    numPostPeriods = 1, Mbarvec = seq(0.5, 2, by = 0.5), # specify the values Mbar
    l_vec = basisVector(index = 1, size = 1)
  )

# Original
originalResults <- HonestDiD::constructOriginalCS(
  betahat = betahat,
  sigma = sigma,
  numPrePeriods = 4,
  numPostPeriods = 1
)

my_labeller <- labeller(method = c("Original" = "Baseline", "C-LF" = "Custom"))

# Sensitivity Plot
sensitivity_plot_rm <- HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults) + 
  # Changing colors for each group
  scale_color_manual(values = c("black", "darkgrey")) + 
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
output_file <- file.path(path_output, paste0("rm_plot_", language, ".png"))
ggsave(output_file, sensitivity_plot_rm, width = 4, height = 3)




