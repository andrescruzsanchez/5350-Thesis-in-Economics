# ------ 5350 Thesis in Economics ------ #             
# ---- Andres Cruz (25199) and Klara Holmer (25037) ---- #

# -------- Skolenkäten --------  #

# Load the data
skolenkäten <- read_excel("skolenkäten.xlsx", sheet = 'Skolenkäten')

# Ensure year_semester is an ordered factor for correct plotting
skolenkäten <- skolenkäten %>%
  mutate(year_semester = factor(year_semester, levels = unique(year_semester)))

# Question of interest
# 2. Stimulans
# 11. Studiero
# 12. Trygghet
# 14. Elevhälsa

# Define question to analyze
question_to_analyze <- "Q14"  # Change this to Q2, Q11, Q12, Q14 as needed

# Create the plot dynamically
question_plot <- ggplot(skolenkäten, 
                        aes_string(x = "year_semester", y = question_to_analyze, 
                                   group = "educational_stage", 
                                   color = "educational_stage", 
                                   shape = "educational_stage")) +
  
  geom_vline(xintercept = 10, linetype = "solid", color = "brown1", size = 0.25) +
  geom_line(size = 0.25) +
  geom_point(size = 1) +
  
  scale_color_manual(values = c("grundskola" = "darkgrey", "gymnasieskola" = "black"),
                     labels = c("grundskola" = "Lower Secondary, Grade 9", 
                                "gymnasieskola" = "Upper Secondary, Grade 11")) + 
  scale_shape_manual(values = c("grundskola" = 21, "gymnasieskola" = 21),
                     labels = c("grundskola" = "Lower Secondary, Grade 9", 
                                "gymnasieskola" = "Upper Secondary, Grade 11")) +
  
  xlab("Academic Year Semester") +
  ylab(paste("Indexvalue", question_to_analyze)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  my_theme

# Print the plot
print(question_plot)

# Save the plot with a dynamic file name
path_output <- "~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/Skolenkäten"
output_file <- file.path(path_output, paste0("Average_DiD_plot_", question_to_analyze, ".png"))
ggsave(output_file, question_plot, width = 4, height = 3)

