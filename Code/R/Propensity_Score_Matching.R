# ------ 5350 Thesis in Economics ------ #             
# ---- Andres Cruz (25199) and Klara Holmer (25037) ---- #
# -- Propensity Score Matching -- #


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

# Source Custom Themes (make sure the path is correct)
source("/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Code/R/Themes.R")

# Define the output path dynamically based on the language variable
path_output <- file.path("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/Propensity Score Matching")

# Reading school data excel file 
school_data <- read_excel("School_Data.xlsx", sheet = "All")

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
  filter(academic_year == '18/19') 

# No Matching: Checking covariate imbalance before matching
unmatched_object <- matchit(
  treatment_group ~ graduates_average_grade_points + private_ownership +
    share_active_certified_teachers + share_postsecondary_parents +
    share_foreign_background + share_female_students, 
  data = school_data_pre,
  method = NULL,
  distance = 'glm',
  link = 'logit'
)
summary(unmatched_object)

# Nearest Neighbor Matching: Construct the matched object 
matched_object <- matchit(
  treatment_group ~ graduates_average_grade_points + private_ownership +
    share_active_certified_teachers + share_postsecondary_parents +
    share_foreign_background + share_female_students, 
  data = school_data_pre,
  method = 'nearest', 
  distance = 'glm',
  link = 'logit',            # Using logistic regression to estimate propensity scores
  m.order = "largest",       # Matching starts with the treated units with highest propensity scores
  ratio = 1,                 # Each treated unit is matched to one control unit
  replace = FALSE,            # Do not reuse control units
  exact = ~private_ownership + school_municipality  # Match only within private/public school and municipality
)

# Checking balance after matching
summary(matched_object, un = FALSE)
print(matched_object)

# Create and save the Love Plot
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
print(love_plot)

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("love_plot.png"))
ggsave(output_file, love_plot, bg = 'transparent', width = 5, height = 3)

# Create and save the Histogram plot
histogram_plots <- plot(matched_object, type='hist')
print(histogram_plots)
output_file <- file.path(path_output, paste0("histogram_plot.png"))
png(filename = output_file)
plot(matched_object, type = 'hist')
dev.off()

# Convert matched object to data set 
matched_dataframe <- match.data(matched_object)

# Filter original panel data to include only matched schools, and dropping missing values
school_data <- school_data %>%
  filter(school_ID %in% matched_dataframe$school_ID) 

# Returns TRUE if any NA is found, else FALSE
any_missing <- anyNA(school_data)
print(any_missing)
