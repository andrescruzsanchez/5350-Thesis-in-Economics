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

# ---- School Data English ---- #

# - Data Preparation - #

# Reading school data excel file and importing english sheet
school_data_english <- read_excel("school_data.xlsx", sheet = "English")

# Filtering and dummy variable creation 
school_data_english <- school_data_english %>%
  filter(academic_year != "2020/21", academic_year != "2021/22") %>%
  filter(graduating_students >= 50) %>%
  mutate(treatment_year = ifelse(academic_year == "2019/20", 1, 0), 
         treatment_group = ifelse(educational_stage == "upper secondary school", 1, 0),
         academic_year = as.factor(academic_year),
         DiD = treatment_year*treatment_group)

# Define the output path
path_output_english <- "/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/English/"

# -- Checking the Parallel Trend Assumption -- #

# - Parallel Trend Plot - #

# Time Trend
parallel_trend_data <- school_data_english %>% 
  drop_na() %>% 
  group_by(academic_year, educational_stage) %>% 
  summarise(average_share_students_F_eng = mean(share_students_F_eng))

# Plot 
parallel_trend <- ggplot(parallel_trend_data, 
                         mapping=aes(x=academic_year, y=average_share_students_F_eng, 
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
  geom_line(size = 0.5, alpha = 0.75) +
  # 
  geom_vline(xintercept = 5.5, linetype = "dashed") + 
  # Modify text
  labs(
    title = "Parallel Trend Assumption - English",
    x = "Academic Year",
    y = "Average percent of students with F"
    ) +   
  # My theme
  my_theme

print(parallel_trend)
  
# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output_english, "parallel_trend_english_test.png")

# Save the plot
ggsave(output_file, parallel_trend, bg = "transparent", width=5, height=4)

# - Event Study - #

# Discrete Academic Year Variable
# Treatment 2018/19
treatment_date <- 19

event_study_data <- school_data_english %>% 
  mutate(academic_year_spring = as.numeric(sub(".*/", "", academic_year)))  %>% 
  mutate(
    time_from_treatment = academic_year_spring - treatment_date,
    lead_1 = case_when(time_from_treatment == -1 ~ 1, TRUE ~ 0),
    lead_2 = case_when(time_from_treatment == -2 ~ 1, TRUE ~ 0),
    lead_3 = case_when(time_from_treatment == -3 ~ 1, TRUE ~ 0),
    lead_4 = case_when(time_from_treatment == -4 ~ 1, TRUE ~ 0),

    lag_0 = case_when(time_from_treatment == 0 ~ 1, TRUE ~ 0),
    lag_1 = case_when(time_from_treatment == 1 ~ 1, TRUE ~ 0)
    ) %>% 
  mutate(academic_year_spring = as.factor(academic_year_spring))

# Set the reference (baseline) category to 2018/19 following Björkegren, Svaleryd and Vlachos (2024)
event_study_data$academic_year_spring <- relevel(event_study_data$academic_year_spring, ref="19")
levels(event_study_data$academic_year_spring)

event_study_regression <- lm(share_students_F_eng ~ lead_4 + lead_3 + lead_2 + lead_1 + lag_1, data = event_study_data)
summary <- summary(event_study_regression)

# Order of Coefficients for the event study plot
plot_order <- c('lead_4', 'lead_3', 'lead_2','lead_1', 'lag_1')

# Extracting Coefficients
leadslags_plot <- tibble(
  estimates = c(summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(summary$coefficients[plot_order, "Std. Error"], 0),
  label = c(-4, -3, -2, -1, 1, 0)
)


event_study_plot <- ggplot(data = leadslags_plot,  
                           aes(x = label, y = estimates)) +
  geom_errorbar(aes(ymin = estimates - 1.96 * standard_errors, 
                    ymax = estimates + 1.96 * standard_errors), 
                size = 0.5, width = 0.05, color = "black", alpha = 0.75) +  
  geom_point(shape = 18, size = 2.5, color = "black") + 
  xlab('Years before and after school closures') +
  ylab('Average percent of students with F') +
  geom_hline(yintercept = 0, linetype = "solid", color = 'brown', alpha = 0.75) +  
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  my_theme

event_study_plot

#
#
#
#

# -- Difference-in-Difference Estimation -- #

# Default Standard Errors

# Model 1 - Default DiD Model
model_1 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD, 
                      data = school_data_english)

# Model 2 - Adding share_postsecondary_parents
model_2 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD +
                share_foreign_background, 
              data = school_data_english)

# Model 3 - Adding share_postsecondary_parents
model_3 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD +
                share_foreign_background + share_postsecondary_parents, 
              data = school_data_english)

# Model 4 - Adding share_active_certified_teachers
model_4 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD +
                share_foreign_background + share_postsecondary_parents + 
                share_active_certified_teachers, 
              data = school_data_english)

# Model 5 - Adding type_of_principal
model_5 <- lm(share_students_F_eng ~ treatment_year + treatment_group + DiD +
                share_foreign_background + share_postsecondary_parents + 
                share_active_certified_teachers + type_of_principal, 
              data = school_data_english)

# - Exporting Regression Output - " 

# Use file.path() to ensure correct formatting of the file path
output_file <- file.path(path_output_english, "regression_output_english_1.html")

# Generate the Stargazer table and save it to the specified path
stargazer::stargazer(
  model_1, model_2, model_3, model_4, model_5, 
  model.numbers = FALSE,
  align = TRUE,
  dep.var.caption = "Dependent variable: Y",
  dep.var.labels = "Share of Students with F in English",
  column.labels = c("Model 1", "Model 2", "Model 3", 'Model 4', 'Model 5', 'Model 6'),
  covariate.labels = c("Treatment Year", "Treatment Group", "DID",
                       "Share Foreign Background", "Share Postsecondary Parents",
                       'Share Active Certified Teachers', 'Type of Principal Kommunal'),
  digits = 3,
  style = "aer",
  type = 'html',
  out = output_file  # Use the dynamically defined output path
)

# Print the output file path to confirm
print(output_file)










# Model 1 - Robust Standard Error
model_1_robust <- lm_robust(share_students_F_eng ~ academic_year*treatment_group, 
                            se_type = 'HC1', 
                            data = panel_data_english)

# Model 1 - Clustered Standard Error
model_1_clustered <- plm(share_students_F_eng ~ academic_year*treatment_group, 
                         model = 'within', effect = 'individual',
                         data = panel_data_english)
summary(model_1_clustered)

robust_cov_mat <- plm::vcovHC(model_1_clustered, type = 'HC2', cluster = 'group')
clustered_robust_se <- sqrt(diag(robust_cov_mat))
clustered_robust_se

# obtain clustered standard errors
coeftest(model_1, vcov = function(x) vcovHC(x, cluster = "group", type = "HC1"))













# -- Event Study -- # 

# Convert the DataFrame into a panel data structure
panel_data_english <- pdata.frame(school_data_english, index = c("school_municipality", "academic_year"))












########
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




#
#
#
#
#
#


# --- Matemathics ----
school_data_matematik <- read_excel("school_data.xlsx", sheet = "Matematik")

#
#
#
#
#
#

# --- Swedish ----
school_data_svenska <- read_excel("school_data.xlsx", sheet = "Svenska")




