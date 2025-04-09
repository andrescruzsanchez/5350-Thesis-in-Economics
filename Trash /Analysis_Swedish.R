# ------ 5350 Thesis in Economics ------ #             
# ---- Andres Cruz (25199) and Klara Holmer (25037) ---- #
# -- Dynamic Subject Analysis -- #

# Define the subject code: choose one of "eng", "sv", or "ma"
subject <- "ma"  # Change to "eng" or "ma" as needed

# Create a lookup vector to convert the subject code into a full subject name
subject_names <- c(eng = "English", sv = "Swedish", ma = "Mathematics")
subject_label <- subject_names[[subject]]

# ------ Initializing ------ #

# Clear workspace
rm(list = ls())

# Clear console
cat("\014")

# Set working directory
setwd("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Processed Data")

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
library(rlang)

# Define the path to the output folder dynamically using the full subject name
path_output <- file.path("/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output", subject_label)

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
  legend.key = element_blank(),  # Removes background behind each legend key
  
  # Adjust axis parameters
  axis.text = element_text(size = 7, color = "black"),
  axis.title = element_text(size = 7, color = "black"),
  axis.ticks = element_line(colour = "black", size = 0.25),
  axis.title.y = element_text(vjust = +3),
  axis.title.x = element_text(vjust = -3),
  
  # Lighter axis lines
  axis.line = element_line(colour = "black", size = 0.25),
  
  # Grid lines settings
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = 'lightgrey', linetype ="solid", size = 0.25),
  
  # Background settings
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

# Reading school data Excel file and importing the sheet using the full subject name
school_data <- read_excel("school_data_ämnen.xlsx", sheet = subject_label)

# Modifying DataFrame
school_data <- school_data %>%
  
  # Rename the dependent variable dynamically.
  # Assumes your columns are named "share_students_F_eng", "share_students_F_sv", "share_students_F_ma"
  rename(share_students_F = !!sym(paste0("share_students_F_", subject))) %>%
  
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
#
#

# ------ Summary Statistics ------ #

# - Ownership - #

# Prepare the summary table
ownership_table <- school_data %>%
  group_by(academic_year, private_ownership) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(
    names_from = private_ownership,
    values_from = Count,
    names_prefix = "Private_ownership_"
  ) %>%
  arrange(academic_year)

# Transpose the table
ownership_table_t <- ownership_table %>%
  pivot_longer(
    cols = c(Private_ownership_0, Private_ownership_1),
    names_to = "Ownership Type",
    values_to = "Count"
  ) %>%
  pivot_wider(
    names_from = academic_year,
    values_from = Count
  )

# Create LaTeX table with dynamic caption
ownership_table_t %>%
  kbl(format = "latex", booktabs = TRUE,
      caption = paste("Number of Schools by Ownership Type and Academic Year for", subject_label)) %>%
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
      caption = paste("Number of Schools by Educational Stage and Academic Year for", subject_label)) %>%
  kable_styling(latex_options = c("hold_position"))

#
#
#
#

# - Missing by size - #

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
      caption = paste("Missing Values in Share of Students with F, by School Size for", subject_label),
      align = "lrrr") %>%
  kable_styling(latex_options = c("hold_position"))

#
#
#
#

# ------ Last Filter ------ #

school_data <- school_data %>%
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
    fill = NA,             # No fill
    color = "black",       # Outline and whisker color
    size = 0.25,           # Thickness of box/whisker lines
    outlier.color = "black",
    outlier.shape = 1,
    outlier.size = 1) + 
  labs(
    title = paste("Boxplot of Share of Students with F in", subject_label),
    x = NULL,
    y = paste("Share of Students with F in", subject_label)) +
  scale_x_discrete(
    labels = c(
      "grundskola" = "Lower Secondary School Year 9",
      "gymnasieskola" = "Upper Secondary School Year 10")
  )

#
#
#
#

# ----- Balancing my Data ----- #
n_periods <- school_data %>%
  distinct(academic_year) %>%
  nrow()

school_data <- school_data %>%
  group_by(school_ID) %>%
  filter(n_distinct(academic_year) == n_periods) %>%
  ungroup()

#
#
#
#

# ------ Checking for Outliers (Post-Balancing) ------ #
ggplot(
  as.data.frame(school_data) %>%
    filter(!is.na(share_students_F)) %>%
    mutate(educational_stage = as.factor(as.character(educational_stage))),
  aes(x = educational_stage, y = share_students_F)) +
  
  my_theme + 
  geom_boxplot(
    fill = NA,
    color = "black",
    size = 0.25,
    outlier.color = "black",
    outlier.shape = 1,
    outlier.size = 1) + 
  labs(
    title = paste("Boxplot of Share of Students with F in", subject_label),
    x = NULL,
    y = paste("Share of Students with F in", subject_label)) +
  scale_x_discrete(
    labels = c(
      "grundskola" = "Lower Secondary School Year 9",
      "gymnasieskola" = "Upper Secondary School Year 10")
  )

#
#
#
#

# ------ Converting to Panel Data Structure ------ #
school_data <- pdata.frame(school_data, index = c("school_ID", "academic_year"))
is.pbalanced(school_data)

#
#
#
#

# ------ Parallel Trend Assumption ------ #

# ---- Parallel Trend Plot ---- #
time_trend <- school_data %>% 
  drop_na() %>% 
  group_by(academic_year_spring, educational_stage) %>% 
  summarise(average_share_students_F = mean(share_students_F))

parallel_trend_plot <- ggplot(time_trend, 
                              mapping = aes(x = academic_year_spring, y = average_share_students_F, 
                                            group = educational_stage, color = educational_stage, shape = educational_stage)) + 
  geom_vline(xintercept = 19.5, linetype = "solid", color = "brown", size = 0.25) +
  geom_line(size = 0.25) +
  geom_point(size = 1) +
  scale_color_manual(values = c("grundskola" = "darkgrey", "gymnasieskola" = "black"),
                     labels = c("grundskola" = "Lower Secondary, Grade 9", 
                                "gymnasieskola" = "Upper Secondary, Grade 10")) + 
  scale_shape_manual(values = c("grundskola" = 21, "gymnasieskola" = 21),
                     labels = c("grundskola" = "Lower Secondary, Grade 9", 
                                "gymnasieskola" = "Upper Secondary, Grade 10")) +
  scale_x_continuous(breaks = c(15, 16, 17, 18, 19, 20, 21, 22),
                     labels = c("14/15", "15/16", "16/17", "17/18", "18/19", "19/20", "20/21", "21/22")) +
  xlab("Academic Year") +
  ylab(paste("Average percent of students with F in", subject_label)) +
  my_theme

print(parallel_trend_plot)

output_file <- file.path(path_output, "Average_DiD_plot.png")
ggsave(output_file, parallel_trend_plot, bg = "transparent", width = 4, height = 3, dpi = 1800)

#
#
#
#
#
#

# ---- Dynamic Difference-in-Difference ---- #

# Set the reference year
school_data$academic_year <- relevel(school_data$academic_year, ref = "18/19")

# -- Step 1 Modelling -- #
dynamic_DiD_without <- plm(share_students_F ~ academic_year * treatment_group, 
                           model = "pooling",
                           effect = "individual", 
                           data = school_data)
dynamic_DiD_without_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_without, type = "HC1", cluster = "group")))
dynamic_DiD_without_summary <- summary(dynamic_DiD_without)

dynamic_DiD_with <- plm(share_students_F ~ academic_year * (treatment_group + private_ownership) + 
                          share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
                        model = "within",   
                        effect = "twoways",
                        data = school_data)
dynamic_DiD_with_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_with, type = "HC1", cluster = "group")))
dynamic_DiD_with_summary <- summary(dynamic_DiD_with)

#
#
#
#
#
#

# -- Step 2 Table Output -- #

Dynamic_DiD_models <- list(dynamic_DiD_without, dynamic_DiD_with)
Dynamic_DiD_cse <- list(dynamic_DiD_without_clustered_se, dynamic_DiD_with_clustered_se)

output_file <- file.path(path_output, "Dynamic_DiD_output.html")
years <- c("14/15", "15/16", "16/17", "17/18", "18/19", "19/20", "20/21", "21/22")
keep_vars <- paste0("academic_year", years, ":treatment_group", collapse = "|")

stargazer::stargazer(
  Dynamic_DiD_models,
  se = Dynamic_DiD_cse,
  title = "Dynamic Difference-in-Difference Models",
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "",
  dep.var.labels = paste("Share of Students with F in", subject_label),
  keep = keep_vars,
  covariate.labels = c("Year -5", "Year -4", "Year -3", "Year -2", "Year 0", "Year 1", "Year 2"),
  digits = 3,
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  add.lines = list(
    c("School Controls", "No", "Yes"), 
    c("School Fixed Effects", "No", "Yes"),
    c("Time Fixed Effects", "Yes", "Yes")
  ),
  column.labels = c("Unconditional", "Conditional"),
  column.separate = c(1, 1),
  notes.append = FALSE,
  notes.label = "Note:",
  notes = paste("The academic year 2018/19 is the reference year. Errors are clustered at the school level. Significance levels: * p<0.10, ** p<0.05, *** p<0.01"),
  type = 'html',
  out = output_file
)

#
#
#
#
#
#

# -- Step 3 Creating Figure -- #

plot_order <- c('academic_year14/15:treatment_group', 
                'academic_year15/16:treatment_group', 
                'academic_year16/17:treatment_group', 
                'academic_year17/18:treatment_group', 
                'academic_year19/20:treatment_group',
                'academic_year20/21:treatment_group',
                'academic_year21/22:treatment_group') 

dynamic_DiD_without_result <- tibble(
  estimates = c(dynamic_DiD_without_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_without_clustered_se[plot_order], 0),
  label = c(-5, -4, -3, -2, 0, 1, 2, -1),
  model = "Without Controls"
)

dynamic_DiD_with_result <- tibble(
  estimates = c(dynamic_DiD_with_summary$coefficients[plot_order, "Estimate"], 0),
  standard_errors = c(dynamic_DiD_with_clustered_se[plot_order], 0),
  label = c(-5, -4, -3, -2, 0, 1, 2, -1),
  model = "With Controls"
)

combined_results <- bind_rows(dynamic_DiD_without_result, dynamic_DiD_with_result) %>%
  mutate(model = factor(model, levels = c("Without Controls", "With Controls")))

dynamic_did_plot <- ggplot(data = combined_results, aes(x = label, y = estimates, color = model, shape = model, group = model)) +
  geom_hline(yintercept = 0, linetype = "solid", color = 'brown', size = 0.25) +
  geom_errorbar(aes(ymin = estimates - 1.96 * standard_errors, 
                    ymax = estimates + 1.96 * standard_errors), 
                size = 0.25, width = 0.05, position = position_dodge(width = 0.2)) +
  geom_point(size = 1, position = position_dodge(width = 0.2)) +
  xlab('Years before and after spring 2020 school closures') +
  ylab('Academic Year x Upper Secondary') +
  scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2)) +
  scale_color_manual(values = c("Without Controls" = "darkgrey", "With Controls" = "black")) +
  scale_shape_manual(values = c("Without Controls" = 0, "With Controls" = 5)) +
  my_theme

print(dynamic_did_plot)

output_file <- file.path(path_output, "Dynamic_DiD_plot.png")
ggsave(output_file, dynamic_did_plot, bg = "transparent", width = 4, height = 3, dpi = 1800)

#
#
#
#
#
#

# ---- Static Difference-in-Difference ---- #

model_1 <- plm(share_students_F ~ treatment_year + treatment_group + DiD, 
               model = "pooling",   
               effect = "individual",
               data = school_data)
cse_1 <- sqrt(diag(vcovHC(model_1, type = "HC1", cluster = "group")))

model_6 <- plm(share_students_F ~ treatment_year + treatment_group + DiD + 
                 academic_year * private_ownership + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
               model = "within",   
               effect = "twoways",
               data = school_data)
cse_6 <- sqrt(diag(vcovHC(model_6, type = "HC1", cluster = "group")))

summary(model_6)

#
#
#
#
#
#

# -- Step 2 Creating a Table -- #

DiD_models <- list(model_1, model_6)
DiD_rse <- list(cse_1, cse_6)

output_file <- file.path(path_output, "Static_DiD_output.html")

stargazer::stargazer(
  DiD_models,
  se = DiD_rse,
  title = "Static Difference-in-Difference Models",
  model.numbers = TRUE,
  align = TRUE,
  dep.var.caption = "",
  dep.var.labels = paste("Share of Students with F in", subject_label),
  keep = "DiD",
  covariate.labels = c("Upper Secondary x Post"),
  digits = 3,
  omit.stat = c("rsq", "adj.rsq", "ser", "f"),
  add.lines = list(
    c("School Controls", "No", "Yes"), 
    c("School Fixed Effects", "No", "Yes"),
    c("Time Fixed Effects", "No", "Yes")
  ),
  column.labels = c("Unconditional", "Conditional"),
  column.separate = c(1, 1),
  notes.append = FALSE,
  notes.label = "Note:",
  notes = "Errors are clustered at the school level. Significance levels: * p<0.10, ** p<0.05, *** p<0.01",
  type = 'html',
  out = output_file
)