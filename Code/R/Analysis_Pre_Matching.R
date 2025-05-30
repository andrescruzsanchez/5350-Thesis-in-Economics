# ------ 5350 Thesis in Economics ------ #             
# ---- Andres Cruz (25199) and Klara Holmer (25037) ---- #

# Analysis on the Pre Propensity Score Sample

#
#
#
#
#
#
#
#

# -------- Initializing -------- #

# Define the output path dynamically based on the language variable
path_output <- file.path("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/Before Propensity Score Matching")

#
#
#
#
#
#
#
#

# -------- Pre Match Summary Statistics -------- #

# Define pre and post periods
school_data <- school_data %>%
  mutate(period = case_when(
    academic_year %in% c("14/15", "15/16", "16/17", "17/18", "18/19") ~ "Pre",
    academic_year %in% c("19/20", "20/21") ~ "Post",
    TRUE ~ NA_character_
  ))

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

# Compute unique school counts by treatment group, period, and ownership type
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

# Render LaTeX table
latex_code <- kable(
  final_table,
  format    = "latex",
  booktabs  = TRUE,
  caption   = "Summary Statistics by Educational Level: Pre and Post Remote Instruction Periods",
  label     = "tab:unmatched_summary",
  col.names = c(
    "Outcome / Count",
    "Ctrl Pre", "Ctrl Post",
    "Trt Pre",  "Trt Post"
  ),
  align  = c("l","r","r","r","r"),
  escape = FALSE
)

# Define the output file path
out_file <- file.path(path_output, "summary_statistics_unmatched.text")

# Write the LaTeX table to that file
writeLines(latex_code, con = out_file)

#
#
#
#
#
#
#
#

# -------- Time Trends -------- #


# ---- Average Grade Points ---- #
### THESE VALUES ARE FROM SKOLVERKET DISTINCT EXCEL SHEET,

# Create the wide dataframe
avg_df <- data.frame(
  educational_stage = c("gymnasieskola", "grundskola"),
  `2014/15` = c(14.5, 13.22),
  `2015/16` = c(14.6, 13.18),
  `2016/17` = c(14.8, 13.15),
  `2017/18` = c(14.8, 13.45),
  `2018/19` = c(14.9, 13.52),
  `2019/20` = c(15.0, 13.59),
  `2020/21` = c(15.1, 13.65),
  `2021/22` = c(15.0, 13.48),
  `2022/23` = c(15.0, 13.44),
  `2023/24` = c(14.9, 13.39),
  check.names = FALSE
)

# Convert to long format
df_long <- avg_df %>%
  pivot_longer(
    cols = -educational_stage,
    names_to = "academic_year",
    values_to = "avg"
  )

df_long$academic_year <- factor(df_long$academic_year, levels = unique(colnames(avg_df)[-1]))

# Time Trend
# Plot the Time Trend for each educational stage
time_trend_plot <- ggplot(df_long, mapping = aes(x = academic_year, y = avg, 
                                                    group = educational_stage, color = educational_stage, shape = educational_stage)) + 

  # Data lines and points (these layers expect the plot to have the necessary data)
  geom_line(size = 0.25) + 
  geom_point(size = 1) + 
  # Custom color and shape assignments for grouping variables
  scale_color_manual(values = c("grundskola" = "darkgrey", "gymnasieskola" = "black"),
                     labels = c("grundskola" = "Grade 9", "gymnasieskola" = "Grade 10")) + 
  scale_shape_manual(values = c("grundskola" = 21, "gymnasieskola" = 19), 
                     labels = c("grundskola" = "Grade 9", "gymnasieskola" = "Grade 10")) + 
  # Axis labels
  xlab("Academic Year") + 
  ylab("Average Grade Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(limits = c(12, 16), breaks = seq(12, 16, by = 1)) + 
  # My_theme
  my_theme 

# Print the plot
print(time_trend_plot)

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("time_trend_plot_avg_unmatched.png"))
ggsave(output_file, time_trend_plot, width = 5, height = 4)

# ---- Swedish ---- #
# Time Trend
time_trend <- school_data %>% 
  group_by(academic_year_spring, educational_stage) %>% 
  summarise(average_share_students_F_sv = mean(share_students_F_sv))

# Plot the Time Trend for each educational stage
time_trend_plot <- ggplot(time_trend, mapping = aes(x = academic_year_spring, y = average_share_students_F_sv, 
                                                    group = educational_stage, color = educational_stage, shape = educational_stage)) + 
  # Time Trend Theme
  time_trend_theme + 
  # My_theme
  my_theme + 
  scale_y_continuous(breaks = seq(0, 5, by = 1)) 
    
# Print the plot
print(time_trend_plot)

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("time_trend_plot_sv_unmatched.png"))
ggsave(output_file, time_trend_plot, width = 5, height = 4)

# ---- English ---- #
# Time Trend
time_trend <- school_data %>% 
  group_by(academic_year_spring, educational_stage) %>% 
  summarise(average_share_students_F_eng = mean(share_students_F_eng))

# Plot the Time Trend for each educational stage
time_trend_plot <- ggplot(time_trend, mapping = aes(x = academic_year_spring, y = average_share_students_F_eng, 
                                                    group = educational_stage, color = educational_stage, shape = educational_stage)) + 
  # Time Trend Theme
  time_trend_theme + 
  # My_theme
  my_theme +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) 


# Print the plot
print(time_trend_plot)

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("time_trend_plot_eng_unmatched.png"))
ggsave(output_file, time_trend_plot, width = 5, height = 4)

# ---- Mathematics ---- #
# Time Trend
time_trend <- school_data %>% 
  group_by(academic_year_spring, educational_stage) %>% 
  summarise(average_share_students_F_ma = mean(share_students_F_ma))

# Plot the Time Trend for each educational stage
time_trend_plot <- ggplot(time_trend, mapping = aes(x = academic_year_spring, y = average_share_students_F_ma, 
                                                    group = educational_stage, color = educational_stage, shape = educational_stage)) + 
  # Time Trend Theme
  time_trend_theme + 
  # My_theme
  my_theme +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) 


# Print the plot
print(time_trend_plot)

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("time_trend_plot_ma_unmatched.png"))
ggsave(output_file, time_trend_plot, width = 5, height = 4)

#
#
#
#
#
#
#
#

# ---- Dynamic Difference-in-Difference ---- #

# Panel Data Conversion
school_data <- pdata.frame(school_data, index = c("school_ID", "academic_year"))
# Setting the reference year
school_data$academic_year <- relevel(school_data$academic_year, ref = "18/19")

#
#
#
#
#
#
#
#

# -- Swedish -- #
# Unconditional Model 
dynamic_DiD_without <- plm(
  share_students_F_sv ~ academic_year*treatment_group, 
  model = "within", effect = "twoways", data = school_data)
dynamic_DiD_without_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_without, type = "HC1", cluster = "group"))) # Clustered SE by school
dynamic_DiD_without_summary <- summary(dynamic_DiD_without) # Summary

# Conditional Model
dynamic_DiD_with <- plm(
  share_students_F_sv ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "within", effect = "twoways", data = school_data)
dynamic_DiD_with_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_with, type = "HC1", cluster = "group"))) # Clustered SE by school
dynamic_DiD_with_summary <- summary(dynamic_DiD_with) # Summary

# -- Step 2 Table Output -- #

# List of models
Dynamic_DiD_models <- list(dynamic_DiD_without, 
                           dynamic_DiD_with)

# List of clustered standard errors 
Dynamic_DiD_cse <- list(dynamic_DiD_without_clustered_se, 
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
output_file <- file.path(path_output, paste0("dynamic_did_output_sv_unmatched.text"))

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
  dep.var.caption = "Failure Rates in Subject Swedish",  
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
output_file <- file.path(path_output, paste0("dynamic_did_plot_sv_unmatched.png"))
ggsave(output_file, dynamic_did_plot, width = 5, height = 4)

#
#
#
#
#
#
#
#

# -- English -- #
# Unconditional Model 
dynamic_DiD_without <- plm(
  share_students_F_eng ~ academic_year*treatment_group, 
  model = "within", effect = "twoways", data = school_data)
dynamic_DiD_without_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_without, type = "HC1", cluster = "group"))) # Clustered SE by school
dynamic_DiD_without_summary <- summary(dynamic_DiD_without) # Summary

# Conditional Model
dynamic_DiD_with <- plm(
  share_students_F_eng ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "within", effect = "twoways", data = school_data)
dynamic_DiD_with_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_with, type = "HC1", cluster = "group"))) # Clustered SE by school
dynamic_DiD_with_summary <- summary(dynamic_DiD_with) # Summary

# -- Step 2 Table Output -- #

# List of models
Dynamic_DiD_models <- list(dynamic_DiD_without, 
                           dynamic_DiD_with)

# List of clustered standard errors 
Dynamic_DiD_cse <- list(dynamic_DiD_without_clustered_se, 
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
output_file <- file.path(path_output, paste0("dynamic_did_output_eng_unmatched.text"))

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
  dep.var.caption = "Failure Rates in Subject English",  
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
output_file <- file.path(path_output, paste0("dynamic_did_plot_eng_unmatched.png"))
ggsave(output_file, dynamic_did_plot, width = 5, height = 4)

#
#
#
#
#
#
#
#


# -- Mathematics -- #
# Unconditional Model 
dynamic_DiD_without <- plm(
  share_students_F_ma ~ academic_year*treatment_group, 
  model = "within", effect = "twoways", data = school_data)
dynamic_DiD_without_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_without, type = "HC1", cluster = "group"))) # Clustered SE by school
dynamic_DiD_without_summary <- summary(dynamic_DiD_without) # Summary

# Conditional Model
dynamic_DiD_with <- plm(
  share_students_F_ma ~ academic_year*treatment_group + share_foreign_background + share_active_certified_teachers + share_postsecondary_parents + share_female_students, 
  model = "within", effect = "twoways", data = school_data)
dynamic_DiD_with_clustered_se <- sqrt(diag(vcovHC(dynamic_DiD_with, type = "HC1", cluster = "group"))) # Clustered SE by school
dynamic_DiD_with_summary <- summary(dynamic_DiD_with) # Summary

# -- Step 2 Table Output -- #

# List of models
Dynamic_DiD_models <- list(dynamic_DiD_without, 
                           dynamic_DiD_with)

# List of clustered standard errors 
Dynamic_DiD_cse <- list(dynamic_DiD_without_clustered_se, 
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
output_file <- file.path(path_output, paste0("dynamic_did_output_ma_unmatched.text"))

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
  dep.var.caption = "Failure Rates in Subject Mathematics",  
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
output_file <- file.path(path_output, paste0("dynamic_did_plot_ma_unmatched.png"))
ggsave(output_file, dynamic_did_plot, width = 5, height = 4)

#
#
#
#
#
#
#
#

# -------- Finalize -------- #

# Drop the pdata.frame class
class(school_data) <- "data.frame"

# Resetting reference year to the baseline level
school_data$academic_year <- factor(school_data$academic_year, levels = c("14/15", "15/16", "16/17", "17/18", "18/19", "19/20", "20/21")) 


