# ------ 5350 Thesis in Economics ------ #             
# ---- Andres Cruz (25199) and Klara Holmer (25037) ---- #
# -- Summary Statistics -- #

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
library(knitr)
library(kableExtra)

# Source Custom Themes (make sure the path is correct)
source("/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Code/R/Themes.R")
# Source Propensity Score Matching Data
source("/Users/andrescruz/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Code/R/Propensity_Score_Matching.R")

# Define the output path dynamically based on the language variable
path_output <- file.path("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/Summary Statistics")

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

# Capture the LaTeX code in a character vector
ownership_table_latex <- capture.output({
  ownership_table_t %>%
    kbl(format = "latex", booktabs = TRUE,
        caption = "Number of Schools by Educational Stage, Ownership Type, and Academic Year") %>%
    kable_styling(latex_options = c("hold_position"))
})

# Define the output file path for the ownership table
output_file_ownership <- file.path(path_output, "ownership_table.tex")

# Write the LaTeX code to the file
writeLines(ownership_table_latex, con = output_file_ownership)

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

# Capture the LaTeX code in a character vector
stage_table_latex <- capture.output({
  stage_table %>%
    kbl(format = "latex", booktabs = TRUE,
        caption = "Number of Schools by Educational Stage and Academic Year") %>%
    kable_styling(latex_options = c("hold_position"))
})

# Define the output file path for the stage table
output_file_stage <- file.path(path_output, "stage_table.tex")

# Write the LaTeX code to the file
writeLines(stage_table_latex, con = output_file_stage)

#
#
#
#

# ---- Balanced boxplot Swedish---- #
box_plot <- ggplot(
  as.data.frame(school_data) %>%
    mutate(educational_stage = as.factor(as.character(educational_stage))),
  aes(x = educational_stage, y = share_students_F_sv)) +
  
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
    y = "Share of Students with F Swedish") +
  
  scale_x_discrete(
    labels = c(
      "grundskola" = "Lower Secondary, Grade 9",
      "gymnasieskola" = "Upper Secondary, Grade 10"))

# Print the plot
print(box_plot)

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("box_plot_sv.png"))
ggsave(output_file, box_plot, bg = 'transparent', width = 4, height = 3)

#
#
#
#

# ---- Balanced boxplot English ---- #
box_plot <- ggplot(
  as.data.frame(school_data) %>%
    mutate(educational_stage = as.factor(as.character(educational_stage))),
  aes(x = educational_stage, y = share_students_F_eng)) +
  
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
    y = "Share of Students with F English") +
  
  scale_x_discrete(
    labels = c(
      "grundskola" = "Lower Secondary, Grade 9",
      "gymnasieskola" = "Upper Secondary, Grade 10"))

# Print the plot
print(box_plot)

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("box_plot_eng.png"))
ggsave(output_file, box_plot, bg = 'transparent', width = 4, height = 3)

#
#
#
#

# ---- Balanced boxplot Mathematics ---- #
box_plot <- ggplot(
  as.data.frame(school_data) %>%
    mutate(educational_stage = as.factor(as.character(educational_stage))),
  aes(x = educational_stage, y = share_students_F_ma)) +
  
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
    y = "Share of Students with F Mathematics") +
  
  scale_x_discrete(
    labels = c(
      "grundskola" = "Lower Secondary, Grade 9",
      "gymnasieskola" = "Upper Secondary, Grade 10"))

# Print the plot
print(box_plot)

# Save the plot using a dynamic file name
output_file <- file.path(path_output, paste0("box_plot_ma.png"))
ggsave(output_file, box_plot, bg = 'transparent', width = 4, height = 3)

